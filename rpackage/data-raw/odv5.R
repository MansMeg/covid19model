## code to prepare `DATASET` dataset goes here
library(covidresponser)
library(covid19imperial)
countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

odv5raw <- covidresponser::covidtracker_csv_v5(countries, TRUE)
stopifnot(!any(duplicated(odv5raw)))
full_names <- names(odv5raw)

# Store raw data
usethis::use_data(odv5raw, version = 2, overwrite = TRUE)
odv5 <- odv5raw

# Clean variable names
ig <- stringr::str_detect(names(odv5), pattern = "Flag")
names(odv5)[ig] <- stringr::str_replace(names(odv5)[ig], pattern = "_", ".")
names(odv5) <- unlist(lapply(strsplit(names(odv5), split =  "_"), function(x) x[1]))


# Fix data quality issues (IsGeneral should be 0 if basic level)
var_nms <- names(odv5)[
  which(stringr::str_detect(names(odv5),
                            pattern = "[A-Z][0-9]+\\.Flag$")) - 1
  ]
for(i in seq_along(var_nms)){
  if(var_nms[i] == "E1") next
  zero_bool <- odv5[,var_nms[i]] == levels(odv5[[var_nms[i]]])[1]
  odv5[[paste0(var_nms[i], ".Flag")]][zero_bool] <- -1
}

# Interpolate missing values
odv5 <- odv5[order(odv5$CountryCode, odv5$Date),]
odv5 <- dplyr::group_by(odv5, CountryCode)
odv5 <- tidyr::fill(odv5, C1, C2, C3, C4, C5, C6, C7, C8, H1, H2, H3)
#od <- tidyr::fill(od, S1.IsGeneral, S2.IsGeneral, S3.IsGeneral, S4.IsGeneral, S5.IsGeneral, S6.IsGeneral)

data(ecdc)

odv5$CountryName[odv5$CountryName == "United Kingdom"] <- "United_Kingdom"
odv5 <- dplyr::left_join(odv5,
                       ecdc[, c("date", "Cases", "Deaths", "Countries.and.territories")],
                       by = c("CountryName" = "Countries.and.territories", "Date" = "date"))

colnames(odv5)[colnames(odv5) == "CountryName"] <- "country"
colnames(odv5)[colnames(odv5) == "Date"] <- "date"
colnames(odv5)[colnames(odv5) == "Cases"] <- "cases"
colnames(odv5)[colnames(odv5) == "Deaths"] <- "deaths"


odv5$country <- as.factor(odv5$country)

assert_daily_data(odv5)

usethis::use_data(odv5, version = 2, overwrite = TRUE)





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

