## code to prepare `DATASET` dataset goes here
library(covid19model)
countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

od <- covidresponser::covidtracker_csv(countries, TRUE)
stopifnot(!any(duplicated(od)))
full_names <- names(od)

# Clean variable names
ig <- stringr::str_detect(names(od), pattern = "IsGeneral")
names(od)[ig] <- stringr::str_replace(names(od)[ig], pattern = "_", ".")
names(od) <- unlist(lapply(strsplit(names(od), split =  "_"), function(x) x[1]))


# Fix data quality issues (IsGeneral should be 0 if basic level)
var_nms <- names(od)[which(stringr::str_detect(names(od), pattern = "S[0-9]+\\.IsGeneral$")) - 1]
for(i in seq_along(var_nms)){
  zero_bool <- od[,var_nms[i]] == levels(od[[var_nms[i]]])[1]
  od[[paste0(var_nms[i], ".IsGeneral")]][zero_bool] <- -1
}

# Interpolate missing values
od <- od[order(od$CountryCode, od$Date),]
od <- dplyr::group_by(od, CountryCode)
od <- tidyr::fill(od, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13)
od <- tidyr::fill(od, S1.IsGeneral, S2.IsGeneral, S3.IsGeneral, S4.IsGeneral, S5.IsGeneral, S6.IsGeneral)

ecdc <- readRDS('../data/COVID-19-up-to-date.rds')
ecdc$date <- lubridate::dmy(ecdc$DateRep)

od$CountryName[od$CountryName == "United Kingdom"] <- "United_Kingdom"
od <- dplyr::left_join(od,
                       ecdc[, c("date", "Cases", "Deaths", "Countries.and.territories")],
                       by = c("CountryName" = "Countries.and.territories", "Date" = "date"))
colnames(od)[c(1, 3, 27, 28)] <- c("country", "date", "cases", "deaths")
od$country <- as.factor(od$country)

odv4 <- od

# Create additional variables
odv4$S6plus <- as.character(odv4$S6)
odv4$S6plus[odv4$S6 == "Restrict movement"] <-
  paste0(odv4$S6[odv4$S6 == "Restrict movement"], odv4$S6.IsGeneral[odv4$S6 == "Restrict movement"])
odv4$S1b <- odv4$S1
levels(odv4$S1b) <- list("No measures"=c("No measures", "Recommend closing"), "Require closing"=c("Require closing"))

# Fill out NA for StringencyIndex
odv4 <- odv4[order(odv4$country, odv4$date),]
odv4 <- dplyr::group_by(odv4, country)
odv4 <- tidyr::fill(odv4, StringencyIndex)

assert_daily_data(odv4)
usethis::use_data(odv4, version = 2, overwrite = TRUE)


if(FALSE){
  # Visual inspection
  library(ggplot2)
  var_nms <- names(od)[which(stringr::str_detect(names(od), pattern = "S[0-9]+$"))]
  full_var_nms <- full_names[which(stringr::str_detect(names(od), pattern = "S[0-9]+$"))]

  for (i in seq_along(countries)) {
    for (j in seq_along(var_nms)){
      plt <- ggplot(data = od[od$CountryCode == countries[i],], aes_string(x = "Date", y = var_nms[j])) + geom_point() + ggtitle(countries[i]) + ylab(full_var_nms[j])
      ggsave(plt, filename = paste0("diagnose_plots/", countries[i], "_", var_nms[j], ".jpeg"))
    }
  }
}

