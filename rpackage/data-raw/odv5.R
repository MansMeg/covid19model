## code to prepare `DATASET` dataset goes here
library(covidresponser)
library(covid19model)
countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

odv5raw <- covidresponser::covidtracker_csv_v5(countries, FALSE)
stopifnot(!any(duplicated(odv5raw)))
full_names <- names(odv5raw)

# Store raw data
usethis::use_data(odv5raw, version = 2, overwrite = TRUE)
# data(odv5raw)
odv5 <- odv5raw

date_max <- as.Date("2020-05-01")

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

# Manual corrections in Data
# These will be filled in below
# Portugal did not have any measures in january
# The 18th of March Portugal introduce state of Emegerncy
odv5$C4[odv5$CountryName == "Portugal" & odv5$Date == "2020-01-01"] <- "No measures"
odv5$C4[odv5$CountryName == "Portugal" & odv5$Date == "2020-03-19"] <- "< 10 people"
odv5$C6[odv5$CountryName == "Portugal" & odv5$Date == "2020-01-01"] <- "No measures"
odv5$C6[odv5$CountryName == "Portugal" & odv5$Date == "2020-03-19"] <- "Require not leaving house (minimal exceptions)"
odv5$C7[odv5$CountryName == "Portugal" & odv5$Date == "2020-04-17"] <- "Restrict movement"

# Greece has NAs when no measures should exist
odv5$C4[odv5$CountryName == "Greece" & odv5$Date == "2020-01-01"] <- "No measures"
odv5$C6[odv5$CountryName == "Greece" & odv5$Date == "2020-01-01"] <- "No measures"
odv5$C3[odv5$CountryName == "Greece" & odv5$Date %in% seq.Date(as.Date("2020-02-28"), as.Date("2020-03-08"), by = 1)] <- "Require cancelling"
# Germany has NAs when no measures should exist + an incorrect value/jump
odv5$C6[odv5$CountryName == "Germany" & odv5$Date == "2020-01-01"] <- "No measures"
odv5$C8[odv5$CountryName == "Germany" & odv5$Date == "2020-04-15"] <- "Ban on high-risk regions"
odv5$C8[odv5$CountryName == "Germany" & odv5$Date == "2020-04-16"] <- "Ban on high-risk regions"
# Austria seem to have an incorrect value/jump
odv5$H2[odv5$CountryName == "Austria" & odv5$Date == "2020-04-23"] <- "Testing of anyone showing COVID19 symptoms"

# Austria seem to have an incorrect value/jump
odv5$C7[odv5$CountryName == "Netherlands" & odv5$Date %in% seq.Date(as.Date("2020-03-12"), as.Date("2020-03-14"), by = 1)] <- "Recommend movement restriction"




# Interpolate missing values
odv5 <- odv5[order(odv5$CountryCode, odv5$Date),]
odv5 <- dplyr::group_by(odv5, CountryCode)
odv5 <- tidyr::fill(odv5, C1, C2, C3, C4, C5, C6, C7, C8, H1, H2, H3)
odv5 <- tidyr::fill(odv5, StringencyIndex)

ecdc <- readRDS('../data/COVID-19-up-to-date.rds')
ecdc$date <- lubridate::dmy(ecdc$DateRep)

odv5$CountryName[odv5$CountryName == "United Kingdom"] <- "United_Kingdom"
odv5 <- dplyr::left_join(odv5,
                       ecdc[, c("date", "Cases", "Deaths", "Countries.and.territories")],
                       by = c("CountryName" = "Countries.and.territories", "Date" = "date"))

colnames(odv5)[colnames(odv5) == "CountryName"] <- "country"
colnames(odv5)[colnames(odv5) == "Date"] <- "date"
colnames(odv5)[colnames(odv5) == "Cases"] <- "cases"
colnames(odv5)[colnames(odv5) == "Deaths"] <- "deaths"

# Remove later dates
odv5 <- odv5[odv5$date <= date_max,]

# Cleanup
odv5$country <- as.factor(odv5$country)

#C7: If flag is partial -> recommend movement restrictions
#H2: General testing or nothing

# Model specific variables
# data("odv5")
odv5$C1b <- odv5$C1
levels(odv5$C1b) <- list("No measures"=c("No measures", "Recommend closing"),
                         "Require closing"=c("Require closing (some)", "Require closing (all)"))
odv5$C2b <- odv5$C2
levels(odv5$C2b) <- list("No measures"=c("No measures"),
                         "Recommend closing"=c("Recommend closing ", "Require closing (some)"),
                         "Require closing"=c("Require closing (all)"))
odv5$C3b <- odv5$C3
levels(odv5$C3b) <- list("No measures"=c("No measures", "Recommend cancelling"),
                         "Require cancelling"=c("Require cancelling"))
odv5$C4b <- odv5$C4
levels(odv5$C4b) <- list("No measures"=c("No measures"),
                         ">= 10 people"=c("> 1000 people", "100-1000 people", "10-100 people"),
                         "< 10 people" = "< 10 people")
odv5$C5b <- odv5$C5
levels(odv5$C5b) <- list("No measures"=c("No measures"),
                         "Recommend closing"=c("Recommend closing", "Require closing"))
odv5$C6b <- odv5$C6
levels(odv5$C6b) <- list("No measures"=c("No measures"),
                         "Recommend not leaving house"=c("Recommend not leaving house"),
                         "Require not leaving house"=c("Require not leaving house", "Require not leaving house (minimal exceptions)"))
odv5$H2b <- odv5$H2
levels(odv5$H2b) <- list("Limited Testing"=c("No testing policy", "Only testing those who both (a) have symptoms, and (b) meet specific criteria"),
                         "Extensive Testing"=c("Testing of anyone showing COVID19 symptoms", "Open public testing"))

assert_daily_data(odv5)
check_daily_data(x = odv5)
usethis::use_data(odv5, version = 2, overwrite = TRUE)

# Add google mobility data
google_file <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
tmpf <- tempfile()
download.file(google_file,destfile = tmpf)
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

odv5$country <- as.character(odv5$country)
odv5g <- dplyr::left_join(odv5, g,
                          by = c("country" = "country_region", "date" = "date"))

odv5g <- odv5g[order(odv5g$country, odv5g$date),]
odv5g <- dplyr::group_by(odv5g, CountryCode)
odv5g <- tidyr::fill(odv5g,
                     country_region_code,
                     retail_and_recreation,
                     grocery_and_pharmacy,
                     parks,
                     transit_stations,
                     workplaces,
                     residential,
                     .direction = "downup")
odv5g$country <- as.factor(odv5g$country)

# Manual fix of variable
odv5g$neg_log_transit_proportion <- -log(pmin((1+odv5g$transit_stations/100), 1))

assert_daily_data(odv5g)
usethis::use_data(odv5g, version = 2, overwrite = TRUE)


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

