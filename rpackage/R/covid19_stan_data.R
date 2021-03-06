#' Create input data to the COVID19 Stan model
#'
#' @param formula a R formula for the effect of covariates in [daily_data] on Rt
#' @param daily_data a data.frame that contains variables
#'        by date, such as cases, deaths and date-specific covariates.
#' @param country_data a data.frame that contains variables
#'        by by country, such as [ifr] and [total_population]
#' @param serial_interval Pre-calculated SI using emprical data from Neil
#' @param ecdf_time ECDF for infection-to-death
#' @param N0 days of observed data + # of days to forecast
#' @param N2 number of days for which to impute infections
#'
#' @details
#' [daily_data] must include the following variables:
#' * \code{date}
#' * \code{country}
#' * \code{cases} (the number of cases for that country and date)
#' * \code{deaths} (the number of deaths for that country and date)
#'
#' [country_data] must include the following variables:
#' * \code{country} (the same countries as in [daily_data])
#' * \code{ifr} (ifr per country)
#' * \code{total_population}
#'
#' @export
covid19_stan_data <- function(formula,
                              daily_data,
                              country_data,
                              serial_interval,
                              ecdf_time,
                              N0 = 6,
                              N2 = 90,
                              formula_hiearchical = NULL,
                              verbose = TRUE){
  checkmate::assert_formula(formula)
  assert_daily_data(daily_data)
  
  countries <- levels(daily_data$country)

  assert_country_data(country_data)
  checkmate::assert_set_equal(country_data$country, countries)

  checkmate::assert_numeric(serial_interval, min.len = N2)
  checkmate::assert_true(sum(serial_interval) <= 1)

  checkmate::assert_class(ecdf_time, c("ecdf", "stepfun", "function"))

  checkmate::assert_int(N0, lower = 0)
  checkmate::assert_int(N2, lower = 5)
  
  if(!is.null(formula_hiearchical)){
    checkmate::assert_formula(formula_hiearchical)
    checkmate::assert_subset(attr(terms(formula_hiearchical),"term.labels"),
                             attr(terms(formula),"term.labels"), empty.ok = TRUE)  
  }

  ### Data munging
  row.names(country_data) <- country_data$country

  d1 <- get_epidemic_period_data(daily_data,
                                 number_of_deaths = 10,
                                 days_before_to_include_in_period = 30)
  EpidemicStart <- get_EpidemicStart(daily_data,
                                     number_of_deaths = 10,
                                     days_before_to_include_in_period = 30)

  Xs <- covid_stan_covariate_data(formula, daily_data = d1, N2 = N2)

  hiearchical <- identify_hiearchical_parameters(formula_hiearchical, Xs, verbose)
  
  Ns <- dplyr::summarise(dplyr::group_by(d1, country), N = dplyr::n())

  f <- lapply(country_data[countries, "ifr"],
              probability_of_death_given_infection,
              ecdf_time = ecdf_time,
              N2 = N2)
  f <- do.call(cbind, f)

  # Create stan data object
  sd <- list()
  sd$M <- length(countries)
  sd$N <- Ns$N
  names(sd$N) <- Ns$country
  sd$deaths <- as_country_matrix(d1$deaths, d1$country, N2, fill_value = -1)
  sd$f <- f
  sd$N0 <- N0
  sd$cases <- as_country_matrix(d1$cases, d1$country, N2, fill_value = -1)
  sd$SI <- serial_interval[1:N2]
  sd$EpidemicStart <- EpidemicStart
  sd$pop <- country_data[countries, "total_population"]
  names(sd$pop) <- country_data[countries, "country"]
  sd$N2 <- N2
  sd$x <- 1:N2
  sd$P <- dim(Xs)[3]
  sd$X <- Xs
  sd$hiearchical <- hiearchical

  if(length(sd$N) == 1) {
    sd$N = as.array(sd$N)
  }

  assert_stan_data(sd)
  
  sd
}

#' Extract design matrices compatible with current stan model
#' @inheritParams covid19_stan_data
#' @export
covid_stan_covariate_data <- function(formula, daily_data, N2 = NULL){
  checkmate::assert_formula(formula)
  assert_daily_data(daily_data)
  checkmate::assert_int(N2, null.ok = TRUE, lower = 1)

  countries <- levels(daily_data$country)

  dat <- model.matrix(formula, data = daily_data)
  if(nrow(dat) != nrow(daily_data)){
    stop("Missing data/values in daily_data. Aborting.", call. = FALSE)
  }
  
  checkmate::assert_matrix(dat, any.missing = FALSE, .var.name = "Design matrix")

  intercept_idx <- which(colnames(dat) == "(Intercept)")
  if(length(intercept_idx) == 1){
    dat <- dat[,-intercept_idx, drop = FALSE]
  } else {
    stop("Model needs to include an intercept (R0).", call. = FALSE)
  }
  dats <- list()
  for(i in seq_along(countries)){
    country <- countries[i]
    tmp <- dat[daily_data$country == country, ,drop = FALSE]
    if(!is.null(N2)){
      N <- nrow(tmp)
      pad_matrix <- matrix(rep(tmp[N,,drop = TRUE], N2 - N), ncol = ncol(tmp), byrow = TRUE)
      tmp <- rbind(tmp, pad_matrix)
    }
    dats[[countries[i]]] <- tmp
  }
  for(i in seq_along(dats)){
    checkmate::assert_true(all(dim(dats[[1]]) == dim(dats[[i]])), .var.name = names(dats)[i])
    checkmate::assert_true(all(colnames(dats[[1]]) == colnames(dats[[i]])), .var.name = names(dats)[i])
  }
  # [1] 15 90  6
  dats <- array(unlist(dats),
                dim = c(nrow(dats[[1]]), ncol(dats[[1]]), length(dats)),
                dimnames = list(rownames(dats[[1]]), colnames(dats[[1]]), names(dats)))
  dats <- aperm(dats, c(3,1,2))
  dats
}

as_country_matrix <- function(x, country, N2, fill_value = -1){
  checkmate::assert_factor(country)
  xlst <- split(x, country)
  xlst <- lapply(xlst, function(x, N2, fill_value) {
    mat <- matrix(fill_value, nrow = N2, ncol = 1)
    mat[1:length(x),] <- x
    mat
  }, N2 = N2, fill_value = fill_value)
  mat <- do.call(cbind, xlst)
  colnames(mat) <- names(xlst)
  mat[,levels(country)]
}


#' Compute probability of death at day i given infection
#'
#' @param ifr The overall probability of dying given infection
#' @param ecdf_time Cumulative distribution over time
#' @param N2 total length of f
#'
#' @export
probability_of_death_given_infection <- function(ifr, ecdf_time, N2){
  # IFR is the overall probability of dying given infection
  convolution = function(u) (ifr * ecdf_time(u))

  f = rep(0, N2) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0))
  for(i in 2:N2) {
    f[i] = (convolution(i+.5) - convolution(i-.5))
  }
  f
}


#' Assert the data formats are correct
#'
#' @param x an object to check
#'
#' @export
assert_stan_data <- function(x){
  checkmate::assert_list(x)
  checkmate::assert_names(names(x),
                          must.include = c("M", "N", "deaths", "f", "N0", "cases", "SI", "EpidemicStart", "pop", "N2", "x", "P", "X"))

}

#' @rdname assert_stan_data
#' @export
assert_daily_data <- function(x){
  checkmate::assert_data_frame(x, .var.name = "daily_data")
  checkmate::assert_names(colnames(x), must.include = c("date", "country", "cases", "deaths"))
  checkmate::assert_factor(x$country)
  checkmate::assert_factor(x$country, any.missing = FALSE)
  countries <- levels(x$country)
  checkmate::assert_date(x$date, any.missing = FALSE)
  checkmate::assert_true(all(table(x$country) > 5))
  for(country in countries){
    # Assert all intermediate days exist in daily data
    min_date <- min(x$date[x$country == country])
    max_date <- max(x$date[x$country == country])
    full_date_sequence <- seq.Date(min_date, max_date, by = 1)
    checkmate::assert_set_equal(x$date[x$country == country],
                                full_date_sequence,
                                .var.name = paste0("daily_data$date[daily_data$country == '",country,"']"))
  }
  checkmate::assert_false(any(duplicated(x)))
  
}

#' Access dates for the epidemic period by country
#'
#' @inheritParams covid19_stan_data
#'
#' @export
get_dates <- function(daily_data){
  assert_daily_data(daily_data)
  d1 <- get_epidemic_period_data(daily_data,
                                 number_of_deaths = 10,
                                 days_before_to_include_in_period = 30)
  split(d1$date, d1$country)
}

#' @rdname covid19_stan_data
#' @export
get_reported_cases <- function(daily_data){
  assert_daily_data(daily_data)
  d1 <- get_epidemic_period_data(daily_data,
                                 number_of_deaths = 10,
                                 days_before_to_include_in_period = 30)
  split(d1$cases, d1$country)
}

#' @rdname covid19_stan_data
#' @export
get_deaths <- function(daily_data){
  assert_daily_data(daily_data)
  d1 <- get_epidemic_period_data(daily_data,
                                 number_of_deaths = 10,
                                 days_before_to_include_in_period = 30)
  split(d1$deaths, d1$country)
}

#' @rdname covid19_stan_data
#' @export
get_countries <- function(stan_data){
  assert_stan_data(stan_data)
  dimnames(stan_data$X)[[1]]
}


#' @rdname assert_stan_data
#' @export
assert_country_data <- function(x){
  checkmate::assert_data_frame(x)
  checkmate::assert_names(colnames(x), must.include = c("country", "total_population", "ifr"))
  checkmate::assert_names(colnames(x), must.include = c("country", "total_population", "ifr"))
  checkmate::assert_false(any(duplicated(x)))
}

#' @rdname covid19_stan_data
#' @export
identify_hiearchical_parameters <- function(formula_hiearchical, X, verbose = TRUE){
  checkmate::assert_formula(formula_hiearchical, null.ok = TRUE)
  checkmate::assert_array(X)
  covariate_names <- dimnames(X)[[3]]
  
  # Function to convert to integer array
  out <- function(x, cn){
    x <- as.integer(x)
    names(x) <- cn
    as.array(x)
  }
  
  if(is.null(formula_hiearchical)){
    bool <- rep(FALSE, length(covariate_names))
    return(out(bool, covariate_names))
  }
  
  tl <- attr(terms(formula_hiearchical), "term.labels")
  bool <- rep(FALSE, length(covariate_names))
  for(i in seq_along(tl)){
    bool <- bool | grepl(x = covariate_names, pattern = paste0("^", tl[i]))
  }
  if(verbose) message("The following parameters will have hiearchical priors:\n", paste0(covariate_names[bool], collapse = ",\n"))
  
  out(bool, covariate_names)

}

