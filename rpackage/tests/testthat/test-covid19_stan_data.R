context("covid19_stan_data")

# Setup data
set.seed(4711)
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = EnvStats::rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = EnvStats::rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution
ecdf.saved = ecdf(x1+x2)



test_that("covid19_stan_data returns a correct object", {
  load(test_path("data/reference_stan_data.rda"))
  load(test_path("data/stan_data_input.rda"))

  expect_silent(
    sd <- covid19_stan_data(formula = ~ schools...universities + self.isolating.if.ill + public.events + any.intervention + lockdown + social.distancing.encouraged,
                            daily_data = daily_data,
                            country_data = country_data,
                            serial_interval = serial_interval,
                            ecdf_time = ecdf.saved, 
                            N0 = 6, 
                            N2 = 90)
  )
  
  expect_equal(names(sd), names(stan_data))
  expect_equal(sd$M, stan_data$M)
  expect_equivalent(sd$N, stan_data$N)
  expect_equivalent(sd$deaths, stan_data$deaths)
  expect_equivalent(sd$f, stan_data$f)
  expect_equal(sd$N0, stan_data$N0)
  # Known bug in Finland and Greece data corrected with new implementation
  expect_equivalent(sd$cases[,-c(12,15)], stan_data$cases[,-c(12,15)])
  expect_equal(sd$SI, stan_data$SI)
  expect_equivalent(sd$EpidemicStart, stan_data$EpidemicStart)
  expect_equivalent(sd$pop, stan_data$pop)
  expect_equivalent(sd$N2, stan_data$N2)
  expect_equivalent(sd$x, stan_data$x)
  expect_equivalent(sd$P, stan_data$P)
  expect_equivalent(sd$X[-c(4,15),,], stan_data$X[-c(4,15),,])
  
  
  
    
})


test_that("covid19_stan_data returns a correct object", {
  load(test_path("data/daily_test_data.rda"))
  load(test_path("data/stan_data_input.rda"))
  
  expect_error(
    sd <- covid19_stan_data(formula = ~ StringencyIndex,
                            daily_data = daily_test_data,
                            country_data = country_data,
                            serial_interval = serial_interval,
                            ecdf_time = ecdf.saved, 
                            N0 = 6, 
                            N2 = 90)
  )
  
  daily_test_data$StringencyIndex[is.na(daily_test_data$StringencyIndex)] <- 0
  
  expect_silent(
    sd <- covid19_stan_data(formula = ~ StringencyIndex,
                            daily_data = daily_test_data,
                            country_data = country_data,
                            serial_interval = serial_interval,
                            ecdf_time = ecdf.saved, 
                            N0 = 6, 
                            N2 = 90)
  )
  
  countries <- c("Spain", "United_Kingdom", "Switzerland")
  for(i in seq_along(countries)){
    country <- countries[i]
    N <- sd$N[country]
    stridx <- daily_test_data$StringencyIndex[daily_test_data$country == country]
    stridx <- stridx[(length(stridx) - N + 1):length(stridx)]
    stridx[N:90] <- stridx[N]
    expect_equivalent(unname(sd$X[country,,"StringencyIndex"]),
                      stridx, info = country)
  }
  
})
