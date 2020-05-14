#' Access and parse ECDC data
#'
#' @references
#' \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
#' @export
get_ecdc_data <- function(){
  # date_offset <- 0
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  # See
  #

  temp_file <- tempfile(fileext = ".csv")
  tryCatch({
    # Download the dataset from the ECDC website to a local temporary file
    r <- httr::RETRY("GET", "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                     httr::write_disk(temp_file, overwrite=TRUE))

    if (httr::http_error(r)) {
      stop("Error downloading file")
    }
  },
  error = function(e) {
    stop(sprintf("Error downloading file '%s': %s, please check %s",
                 url, e$message, url_page))
  })

  d <- read.csv(temp_file, stringsAsFactors = FALSE)
  d$t <- lubridate::decimal_date(as.Date(d$dateRep, format = "%d/%m/%Y"))
  d <- d[order(d$'countriesAndTerritories', d$t, decreasing = FALSE), ]
  names(d)[names(d) == "countriesAndTerritories"] <- "Countries.and.territories"
  names(d)[names(d) == "deaths"] <- "Deaths"
  names(d)[names(d) == "cases"] <- "Cases"
  names(d)[names(d) == "dateRep"] <- "DateRep"
  d
}



