#' Access google mobility data
#'
#' @export
get_data_google_mobility <- function(){
  google_file <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  tmpf <- tempfile()
  download.file(google_file, destfile = tmpf)
  g <- read.csv(tmpf, stringsAsFactors = FALSE)
  g <- g[!is.na(g$country_region),]
  g$date <- as.Date(g$date)
  g$country_region[g$country_region == "United Kingdom"] <- "United_Kingdom"

  g <- g[g$country_region %in% levels(odv5$country),]
  g <- g[g$sub_region_1 == "",]
  g <- g[g$sub_region_2 == "",]
  names(g) <- stringr::str_replace_all(names(g), pattern = "_percent_change_from_baseline", replacement = "")
  g$sub_region_1 <- NULL
  g$sub_region_2 <- NULL
  g
}
