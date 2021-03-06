library(rstan)
suppressPackageStartupMessages(library(EnvStats))
suppressPackageStartupMessages(library(optparse))
# remotes::install_local("rpackage", force = TRUE)
library(covid19model)

start_time <- Sys.time()

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
if(is.null(cmdoptions$options$debug)) {
  # Sys.setenv(DEBUG = "TRUE")
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}

if(DEBUG) {
  print("Running in DEBUG mode")
} else  {
  print("Running in FULL mode")
}

# Read in config
cfg_path <- cmdoptions$args[1]
# cfg_path <- "triton/configs/test_model.yml"
# cfg_path <- "triton/configs/test_base_general.yml"
cfg <- yaml::read_yaml(cfg_path)
cat(yaml::as.yaml(cfg))

# Read in libraries
if (!is.null(cfg$libraries)){
  for(i in seq_along(cfg$libraries)){
    eval(parse(text = paste0("library(", cfg$libraries[i], ")")))
  }
}

## Ensure that output directories exist
dir.create("results/", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/", showWarnings = FALSE, recursive = TRUE)

## Reading all data
data(country_data)
serial.interval <- read.csv("data/serial_interval.csv")
serial_interval <- c(serial.interval$fit, rep(0, 100))

# Read in daily_data from cfg
eval(parse(text = paste0("data(", cfg$data$daily_data, ")")))
eval(parse(text = paste0("daily_data <- ", cfg$data$daily_data)))

# Parameters
N2 = cfg$model_arguments$N2 # increase if you need more forecast
date_min <- as.Date(cfg$model_arguments$date$start)
date_max <- as.Date(cfg$model_arguments$date$end)

# Extract dates in analysis
daily_data <- daily_data[daily_data$date <= date_max,]

# Replace missing values with 0
daily_data$cases[is.na(daily_data$cases)] <- 0
daily_data$deaths[is.na(daily_data$deaths)] <- 0

assert_daily_data(daily_data)


set.seed(cfg$model_arguments$seed)
# various distributions required for modeling
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = EnvStats::rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = EnvStats::rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution

ecdf.saved = ecdf(x1+x2)


# Note that the Stan model already includes an intercept
formula_from_cfg <- eval(parse(text = cfg$model_arguments$model_formula))
if(!is.null(cfg$model_arguments$model_formula_hiearchical)){
  formula_hiearchical_from_cfg <- eval(parse(text = cfg$model_arguments$model_formula_hiearchical))
} else {
  formula_hiearchical_from_cfg <- NULL
}
stan_data <- covid19_stan_data(formula = formula_from_cfg,
                               formula_hiearchical = formula_hiearchical_from_cfg,
                               daily_data = daily_data,
                               country_data = country_data,
                               serial_interval = serial_interval,
                               ecdf_time = ecdf.saved, 
                               N0 = 6, 
                               N2 = N2)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',
                      cfg$model_arguments$stan_model))


if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40, warmup=20, chains=2, seed = 4711)
} else {
  fit = sampling(m, data=stan_data, 
                 iter = cfg$stan_arguments$iter,
                 warmup = cfg$stan_arguments$warmup,
                 chains = cfg$stan_arguments$chains,
                 thin = cfg$stan_arguments$thin,
                 control = cfg$stan_arguments$control)
}

JOBID = cfg$job_id
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

runtime <-  list(start = start_time,
                 end = Sys.time())
cat("Run time:\n")
print(runtime$end - runtime$start)

git_sha <- try(system("git rev-parse HEAD", intern=TRUE))

save(fit, stan_data, daily_data, country_data, cfg, runtime, git_sha, file=paste0('results/', JOBID, '-stanfit.Rdata'))

