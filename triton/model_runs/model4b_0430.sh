#!/bin/bash
#SBATCH --time=0-04:00:00
#SBATCH --cpus-per-task=4

date

export JOB_ID="model4b_0430"

module load r/3.6.1-python3

# Setup yml config file
cat > $JOB_ID.yml <<EOF

job_id: "$JOB_ID"

libraries:
  - splines

model_arguments:
  stan_model: "base_general_speed_alpha.stan"
  model_formula: "~ bs(StringencyIndex, df = 5, degree = 3)"
  N2: 110
  seed: 4711
  date:
    start: "2020-01-01"
    end: "2020-04-30"

data:
  countries: ["Denmark", "Italy", "Germany", "Spain", "United_Kingdom", "France", "Norway", "Belgium", "Austria", "Sweden", "Switzerland", "Greece", "Portugal", "Netherlands", "Finland"]
  daily_data: "odv4"

stan_arguments:
  iter: 4000
  warmup: 2000
  chains: 4
  thin: 4
  control:
    adapt_delta: 0.95
    max_treedepth: 10

EOF

# Run model
# To debug: Sys.setenv(DEBUG = "TRUE")
Rscript run_model.R $JOB_ID.yml

# Remove config
rm $JOB_ID.yml

date

