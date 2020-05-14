## code to prepare `DATASET` dataset goes here
library(covid19model)
countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

odv5raw <- covidtracker_csv_v5(countries, TRUE)
stopifnot(!any(duplicated(odv5raw)))
full_names <- names(odv5raw)

# Store raw data
dn <- paste0("odv5raw_", substr(gsub(Sys.Date(), pattern = "-", replacement = ""), 5, 8))
assign(dn, value = odv5raw)
eval(parse(text = paste0("usethis::use_data(", dn, ", version = 2, overwrite = TRUE)")))

# data("odv5raw_0514")

odv5 <- odv5raw

date_max <- as.Date("2020-05-10")

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
odv5$C7[odv5$CountryName == "Portugal" & odv5$Date %in% seq.Date(as.Date("2020-04-09"), as.Date("2020-04-13"), by = 1)] <- "Recommend movement restriction"
odv5$C7[odv5$CountryName == "Portugal" & odv5$Date %in% seq.Date(as.Date("2020-05-01"), as.Date("2020-05-03"), by = 1)] <- "Recommend movement restriction"

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



# Variables currently not in use
odv5 <- tidyr::fill(odv5,
#                   C1.Flag, C2.Flag, C3.Flag, C4.Flag, C5.Flag, C6.Flag, C7.Flag, E1.Flag, H1.Flag,
                    E1, E2, E3, E4,
                    H3, H4, H5,
                    .direction = "downup")

# Remove variables not in use due to large number of missing values
odv5$M1 <- NULL
odv5$E4 <- NULL

# Remove flag variables
flag_idx <- grepl(colnames(odv5), pattern = "Flag")
odv5 <- odv5[, !flag_idx]

odv5 <- tidyr::fill(odv5,
                    ConfirmedCases, ConfirmedDeaths, StringencyIndexForDisplay, LegacyStringencyIndex, LegacyStringencyIndexForDisplay,
                    .direction = "down")
# Additional NA is 0 in the beginning of the series
odv5$ConfirmedCases[is.na(odv5$ConfirmedCases)] <- 0
odv5$ConfirmedDeaths[is.na(odv5$ConfirmedDeaths)] <- 0


ecdc <- get_ecdc_data()
ecdc$date <- lubridate::dmy(ecdc$DateRep)

odv5$CountryName[odv5$CountryName == "United Kingdom"] <- "United_Kingdom"
odv5 <- dplyr::left_join(odv5,
                       ecdc[, c("date", "Cases", "Deaths", "Countries.and.territories")],
                       by = c("CountryName" = "Countries.and.territories", "Date" = "date"))

colnames(odv5)[colnames(odv5) == "CountryName"] <- "country"
colnames(odv5)[colnames(odv5) == "Date"] <- "date"
colnames(odv5)[colnames(odv5) == "Cases"] <- "cases"
colnames(odv5)[colnames(odv5) == "Deaths"] <- "deaths"

# Set missing values in cases and deaths previous days value
odv5 <- tidyr::fill(odv5,
                    cases, deaths,
                    .direction = "down")
# Additional NA is 0 in the beginning of the series
odv5$cases[is.na(odv5$cases)] <- 0
odv5$deaths[is.na(odv5$deaths)] <- 0


# Remove later dates
odv5 <- odv5[odv5$date <= date_max,]

# Cleanup
odv5$country <- as.factor(odv5$country)


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

# Check country
if(FALSE){
  to_check <- odv5[odv5$country == "Portugal" & lubridate::month(odv5$date) >= 2, c("date", "C6")]
}

assert_daily_data(odv5)
check_daily_data(x = odv5)
# Belgium and Switzerland in H2/H2b is correct

# Store data
dn <- paste0("odv5_", substr(gsub(Sys.Date(), pattern = "-", replacement = ""), 5, 8))
assign(dn, value = odv5)
eval(parse(text = paste0("usethis::use_data(", dn, ", version = 2, overwrite = TRUE)")))


# Add google mobility data
g <- get_data_google_mobility()

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
odv5g$StringencyIndex <- odv5g$StringencyIndex / 100
odv5g$StringencyIndexForDisplay <- odv5g$StringencyIndexForDisplay / 100

assert_daily_data(odv5g)


# Store data
dn <- paste0("odv5g_", substr(gsub(Sys.Date(), pattern = "-", replacement = ""), 5, 8))
assign(dn, value = odv5g)
eval(parse(text = paste0("usethis::use_data(", dn, ", version = 2, overwrite = TRUE)")))





## Additional visual inspection -----

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

