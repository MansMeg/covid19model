
countries <- c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  "United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria", 
  "Sweden",
  "Switzerland",
  "Greece",
  "Portugal",
  "Netherlands", 
  "Finland"
)

## Reading all data
d <- readRDS('data/COVID-19-up-to-date.rds')
d$date <- lubridate::dmy(d$DateRep)

covariates <- read.csv('data/interventions.csv', stringsAsFactors = FALSE)
covariates$Date.effective <- as.Date(covariates$Date.effective, format = "%d.%m.%Y")

# Parameters
date_min <- lubridate::dmy('31/12/2019') 
date_max <- max(d$date)

covariates_df <- covariates_to_dataframe(x = covariates, start_date = date_min, end_date = date_max)
covariates_df <- dplyr::left_join(covariates_df, 
                                  d[, c("date", "Cases", "Deaths", "Countries.and.territories")], 
                                  by = c("country" = "Countries.and.territories", "date" = "date"))
covariates_df$country <- factor(covariates_df$country, levels = countries)
colnames(covariates_df) <- tolower(colnames(covariates_df))

# create the `any intervention` covariate
covariates_df$any.intervention = as.integer(
  (covariates_df$schools...universities +
     covariates_df$self.isolating.if.ill +
     covariates_df$public.events + 
     covariates_df$lockdown + 
     covariates_df$social.distancing.encouraged) >= 1)

icv3 <- covariates_df 

assert_daily_data(icv3)

usethis::use_data(icv3, version = 2, overwrite = TRUE)

