ecdc <- readRDS('../data/COVID-19-up-to-date.rds')
ecdc$date <- lubridate::dmy(ecdc$DateRep)

usethis::use_data(ecdc, version = 2, overwrite = TRUE)

