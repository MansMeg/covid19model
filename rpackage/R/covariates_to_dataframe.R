#' Convert the covariates data to a data.frame
#' 
#' @description 
#' The function computes a data.frame from the [interventions.csv] file.
#' 
#' @param x a data.frame with variables Country, Type and Date 
#' @param start_date The starting date to use
#' @param end_date The end date to use
#' @param nrows The total number of days
#' 
#' @export
covariates_to_dataframe <- function(x, start_date = "2020-01-01", end_date = Sys.Date(), nrows = NULL){
  checkmate::assert_data_frame(x)
  checkmate::assert_names(colnames(x), must.include = c("Country", "Type", "Date.effective"))
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  checkmate::assert_date(start_date)
  checkmate::assert_date(end_date)
  checkmate::assert_date(x$Date.effective)
  checkmate::assert_integerish(nrows, lower = 1, null.ok = TRUE)
  

  countries <- unique(x$Country)
  types <- unique(x$Type)
  dat_list <- list()
  for(i in seq_along(countries)){
    if(is.null(nrows)){
      dat <- data.frame(country = countries[i], date = seq(start_date, end_date, by = 1))
    } else {
      dat <- data.frame(country = countries[i], date = start_date + (0:(nrows-1)))
    }
    for(j in seq_along(types)){
      date <- x$Date.effective[x$Country == countries[i] & x$Type == types[j]]
      if(length(date) == 0){
        dat[types[j]] <- rep(0L, nrow(dat))        
      } else {
        dat[types[j]] <- cumsum(as.integer(dat$date == date))
      }
    }
    dat_list[[i]] <- dat
  }
  dat <- do.call(rbind, dat_list)
  names(dat) <- make.names(names(dat))
  dat$country <- as.character(dat$country)
  dat
}
