library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(magrittr)
library(optparse)
# remotes::install_local("rpackage", force = TRUE)
library(covid19imperial)

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

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}

if(is.null(cmdoptions$options$full)) {
  FULL = Sys.getenv("FULL") == "TRUE"
} else {
  FULL = cmdoptions$options$full
}

if(DEBUG && FULL) {
  stop("Setting both debug and full run modes at once is invalid")
}

if(length(cmdoptions$args) == 0) {
  StanModel = 'base_general_speed'
} else {
  StanModel = cmdoptions$args[1]
}
 
print(sprintf("Running %s",StanModel))
if(DEBUG) {
  print("Running in DEBUG mode")
} else if (FULL) {
  print("Running in FULL mode")
}

## Ensure that output directories exist
dir.create("results/", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/", showWarnings = FALSE, recursive = TRUE)


## Reading all data
d <- readRDS('data/COVID-19-up-to-date.rds')
d$date <- dmy(d$DateRep)

# read data
## get IFR and population from same file
ifr.by.country = read.csv("data/popt_ifr.csv")
ifr.by.country$country = as.character(ifr.by.country[,2])
ifr.by.country$country[ifr.by.country$country == "United Kingdom"] = "United_Kingdom"
names(ifr.by.country)[2:4] <- c("country", "total_population", "ifr")

serial.interval = read.csv("data/serial_interval.csv")

# Read in covariate data
data(od)

# Parameters
N2 = 90 # increase if you need more forecast
date_min <- dmy('31/12/2019') 
date_max <- max(d$date)


od$CountryName[od$CountryName == "United Kingdom"] <- "United_Kingdom"
od <- dplyr::group_by(od, CountryCode)
od <- tidyr::fill(od, StringencyIndex)
covariates_df <- dplyr::left_join(od, 
                                  d[, c("date", "Cases", "Deaths", "Countries.and.territories")], 
                                  by = c("CountryName" = "Countries.and.territories", "Date" = "date"))
covariates_df$S6plus <- as.character(covariates_df$S6)
covariates_df$S6plus[covariates_df$S6 == "Restrict movement"] <- 
  paste0(covariates_df$S6[covariates_df$S6 == "Restrict movement"], covariates_df$S6.IsGeneral[covariates_df$S6 == "Restrict movement"])
colnames(covariates_df)[c(1, 3, 23, 24)] <- c("country", "date", "cases", "deaths")
covariates_df$country <- factor(covariates_df$country, levels = countries)
# table(covariates_df$country, is.na(covariates_df$ConfirmedCases))
covariates_df <- covariates_df[covariates_df$date <= date_max,]

covariates_df$cases[is.na(covariates_df$cases)] <- 0
covariates_df$deaths[is.na(covariates_df$deaths)] <- 0


set.seed(4711)
# various distributions required for modeling
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution

ecdf.saved = ecdf(x1+x2)

daily_data = covariates_df
country_data = ifr.by.country
# Note that the Stan model already includes an intercept
stan_data <- covid19_stan_data(formula = ~ -1 + S1 + S2 + S3 + S4 + S5 + S6plus,
                               daily_data = covariates_df,
                               country_data = ifr.by.country,
                               serial_interval = serial.interval$fit,
                               ecdf_time = ecdf.saved, 
                               N0 = 6, 
                               N2 = N2)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan'))


if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40, warmup=20, chains=2, seed = 4711)
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=4000,warmup=2000,chains=4,thin=4,control = list(adapt_delta = 0.95, max_treedepth = 10))
} else { 
  fit = sampling(m,data=stan_data,iter=200,warmup=100,chains=4,thin=4,control = list(adapt_delta = 0.95, max_treedepth = 10))
}  

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

save.image(paste0('results/',StanModel,'-',JOBID,'.Rdata'))

save(fit, daily_data, country_data, file=paste0('results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

if(FALSE){
  
library(bayesplot)
filename <- paste0(StanModel,'-',JOBID)
system(paste0("Rscript covariate-size-effects.r ", filename,'-stanfit.Rdata'))
mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
ggsave(sprintf("results/%s-mu.png",filename),g,width=4,height=6)
tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
Rt_adj = do.call(cbind,tmp)
colnames(Rt_adj) = countries
g = (mcmc_intervals(Rt_adj,prob = .9))
ggsave(sprintf("results/%s-final-rt.png",filename),g,width=4,height=6)
system(paste0("Rscript plot-3-panel.r ", filename,'-stanfit.Rdata'))
system(paste0("Rscript plot-forecast.r ",filename,'-stanfit.Rdata'))
#system(paste0("Rscript make-table.r results/",filename,'-stanfit.Rdata'))
verify_result <- system(paste0("Rscript web-verify-output.r ", filename,'.Rdata'),intern=FALSE)
if(verify_result != 0){
  stop("Verification of web output failed!")
}
}

