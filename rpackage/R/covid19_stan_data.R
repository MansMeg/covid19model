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
                              verbose = TRUE){
  checkmate::assert_formula(formula)
  checkmate::assert_data_frame(daily_data)
  checkmate::assert_names(colnames(daily_data), must.include = attr(terms(formula), "term.labels"))
  checkmate::assert_names(colnames(daily_data), must.include = c("date", "country", "cases", "deaths"))
  checkmate::assert_factor(daily_data$country, any.missing = FALSE)
  countries <- levels(daily_data$country)
  checkmate::assert_date(daily_data$date, any.missing = FALSE)
  for(country in countries){
    # Assert all intermediate days exist in daily data
    min_date <- min(daily_data$date[daily_data$country == country])
    max_date <- max(daily_data$date[daily_data$country == country])
    full_date_sequence <- seq.Date(min_date, max_date, by = 1)
    checkmate::assert_set_equal(daily_data$date[daily_data$country == country], 
                                full_date_sequence, 
                                .var.name = paste0("daily_data$date[daily_data$country == '",country,"']"))
  }
  checkmate::assert_false(any(duplicated(daily_data)))
  checkmate::assert_data_frame(country_data)
  checkmate::assert_names(colnames(country_data), must.include = c("country", "total_population", "ifr"))
  checkmate::assert_names(colnames(country_data), must.include = c("country", "total_population", "ifr"))
  checkmate::assert_false(any(duplicated(country_data)))
  checkmate::assert_set_equal(country_data$country, countries)
  
  checkmate::assert_numeric(serial_interval, min.len = N2)
  checkmate::assert_true(sum(serial_interval) <= 1)
  
  checkmate::assert_class(ecdf_time, c("ecdf", "stepfun", "function"))
  
  checkmate::assert_int(N0, lower = 0)
  checkmate::assert_int(N2, lower = 5)
  
  ### Data munging
  row.names(country_data) <- country_data$country
  
  d1 <- get_epidemic_period_data(daily_data,
                                 number_of_deaths = 10,
                                 days_before_to_include_in_period = 30) 
  EpidemicStart <- get_EpidemicStart(daily_data,
                                     number_of_deaths = 10,
                                     days_before_to_include_in_period = 30) 
  
  Xs <- covid_stan_covariate_data(formula, daily_data = d1, N2 = N2)
  
  Ns <- dplyr::summarise(dplyr::group_by(d1, country), N = n())
  
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
  
  if(length(sd$N) == 1) {
    sd$N = as.array(sd$N)
  }
  
  sd
}


#' Extract design matrices compatible with current stan model
#' @inheritParams covid19_stan_data
#' @export
covid_stan_covariate_data <- function(formula, daily_data, N2 = NULL){
  checkmate::assert_formula(formula)
  checkmate::assert_data_frame(daily_data)
  checkmate::assert_names(colnames(daily_data), must.include = attr(terms(formula), "term.labels"))
  checkmate::assert_names(colnames(daily_data), must.include = c("date", "country"))
  checkmate::assert_factor(daily_data$country)
  checkmate::assert_int(N2, null.ok = TRUE, lower = 1)

  countries <- levels(daily_data$country)
  dat <- list()
  for(i in seq_along(countries)){
    tmp <- daily_data[daily_data$country == countries[i],]
    if(!is.null(N2)){
      N <- nrow(tmp)
      tmp[N:N2,] <- tmp[N,] 
    }
    dat[[countries[i]]] <- model.matrix(formula, tmp)
  }
  # [1] 15 90  6
  dat <- array(unlist(dat), 
               dim = c(nrow(dat[[1]]), ncol(dat[[1]]), length(dat)),
               dimnames = list(rownames(dat[[1]]), colnames(dat[[1]]), names(dat)))
  dat <- aperm(dat, c(3,1,2))
  dat
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
