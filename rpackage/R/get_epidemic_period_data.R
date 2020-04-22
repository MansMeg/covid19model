#' Extract the epidemic period from data
#' 
#' @description 
#' The epidemic is said to start [days_before_to_include_in_period] days before
#' the country has reached [number_of_deaths] number of deaths.
#' 
#' @param x a data.frame with the number of cases and deaths per date and country
#' @param number_of_deaths The number of deaths to define starting of epidemic
#' @param days_before_to_include_in_period The number of days before the cumulative deaths to use as starting date
#' 
#' @export
get_epidemic_period_data <- function(x, 
                                number_of_deaths, 
                                days_before_to_include_in_period){

  idxs <- compute_index(x, 
                        number_of_deaths, 
                        days_before_to_include_in_period)

  x <- dplyr::group_by(x, country)
  x <- dplyr::mutate(x, row_number = row_number())
  x <- dplyr::left_join(x, data.frame(country = names(idxs$index2),
                                      start = idxs$index2, 
                                      stringsAsFactors = FALSE),
                        by = "country")
  x <- dplyr::filter(x, start <= row_number)
  x$start <- NULL
  x$row_number <- NULL
  
  x
}

#' @rdname get_epidemic_period_data
#' @export
get_EpidemicStart <- function(x, 
                              number_of_deaths, 
                              days_before_to_include_in_period){
  idxs <- compute_index(x, 
                        number_of_deaths, 
                        days_before_to_include_in_period)
  EpidemicStart <- idxs$index1 + 1 - idxs$index2
  EpidemicStart
}

compute_index <- function(x, 
                          number_of_deaths, 
                          days_before_to_include_in_period){
  checkmate::assert_data_frame(x)
  checkmate::assert_names(colnames(x), must.include = c("country", "cases", "deaths"))
  checkmate::assert_false(any(is.na(x$cases)))
  checkmate::assert_false(any(is.na(x$deaths)))
  checkmate::assert_integerish(number_of_deaths, lower = 0)
  checkmate::assert_integerish(days_before_to_include_in_period, lower = 0)
  
  x <- dplyr::group_by(x, country)
  x <- dplyr::mutate(x, cumsum_deaths = cumsum(deaths))
  x_country_list <- split(x, x$country)
  index1 <- unlist(lapply(x_country_list, 
                          function(x) which(cumsum(x$deaths) >= number_of_deaths)[1]))
  index2 <- index1 - days_before_to_include_in_period
  return(list(index1 = index1, index2 = index2))
}

