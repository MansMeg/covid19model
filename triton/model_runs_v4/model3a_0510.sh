#!/bin/bash
#SBATCH --time=0-04:00:00
#SBATCH --cpus-per-task=4

date

git rev-parse --short HEAD

export JOB_ID="model3a_0510"

module load r/3.6.1-python3

# Setup yml config file
cat > $JOB_ID.yml <<EOF

job_id: "$JOB_ID"

libraries:
  - splines

model_arguments:
  stan_model: "base_hiearchical.stan"
  model_formula: "~ C1b + C2b + C3b + C4b + C5b + C6b + H2b + neg_log_transit_proportion"
  model_formula_hiearchical: "~ C1b + C2b + C3b + C4b + C5b + C6b + H2b + neg_log_transit_proportion"
  N2: 120
  seed: 4711
  date:
    start: "2020-01-01"
    end: "2020-05-10"

data:
  countries: ["Denmark", "Italy", "Germany", "Spain", "United_Kingdom", "France", "Norway", "Belgium", "Austria", "Sweden", "Switzerland", "Greece", "Portugal", "Netherlands", "Finland"]
  daily_data: "odv5g_0514"

stan_arguments:
  iter: 2000
  warmup: 1000
  chains: 4
  thin: 2
  control:
    adapt_delta: 0.92
    max_treedepth: 10

EOF

# Run model
Rscript run_model.R $JOB_ID.yml

# Remove config
rm $JOB_ID.yml

date

