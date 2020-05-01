library(covid19model)

ifr.by.country = read.csv("../data/popt_ifr.csv")
ifr.by.country$country = as.character(ifr.by.country[,2])
ifr.by.country$country[ifr.by.country$country == "United Kingdom"] = "United_Kingdom"
names(ifr.by.country)[2:4] <- c("country", "total_population", "ifr")
country_data <- ifr.by.country

assert_country_data(country_data)


usethis::use_data(country_data, version = 2, overwrite = TRUE)

