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
data(ecdc)
data(country_data)
data(od)
serial.interval <- read.csv("data/serial_interval.csv")

# Parameters
N2 = 90 # increase if you need more forecast
date_min <- dmy('31/12/2019') 
date_max <- max(ecdc$date)

# Create additional variable
od$S6plus <- as.character(od$S6)
od$S6plus[od$S6 == "Restrict movement"] <- 
  paste0(od$S6[od$S6 == "Restrict movement"], od$S6.IsGeneral[od$S6 == "Restrict movement"])
levels(od$S1) <- list("No measures"=c("No measures", "Recommend closing"), "Require closing"=c("Require closing"))

# Extract dates in analysis
od <- od[od$date <= date_max,]

# Replace missing values with 0
od$cases[is.na(od$cases)] <- 0
od$deaths[is.na(od$deaths)] <- 0


set.seed(4711)
# various distributions required for modeling
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution

ecdf.saved = ecdf(x1+x2)

daily_data = od
# Note that the Stan model already includes an intercept
stan_data <- covid19_stan_data(formula = ~ S1 + S2 + S3 + S4 + S5 + S6plus,
                               daily_data = od,
                               country_data = country_data,
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
  fit = sampling(m,data=stan_data,iter=4000,warmup=2000,chains=4,thin=4,control = list(adapt_delta = 0.99, max_treedepth = 15), seed = 4711)
} else { 
  fit = sampling(m,data=stan_data,iter=200,warmup=100,chains=4,thin=4,control = list(adapt_delta = 0.95, max_treedepth = 10))
}  

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

save(fit, stan_data, daily_data, country_data, file=paste0('results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

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

