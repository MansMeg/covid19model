#!/bin/bash
#SBATCH --time=0-08:00:00
#SBATCH --cpus-per-task=4

date

module load r/3.6.1-python3

# Setup yml config file
cat > config.yml <<EOF

job_id: "model2_0410"

model_arguments:
  stan_model: "base_general_speed.stan"
  model_formula: "~ S1b + S2 + S3 + S4 + S5 + S6plus"
  N2: 90
  seed: 4711
  date:
    start: "2020-01-01"
    end: "2020-04-10"

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
Rscript run_model.R config.yml

# Remove config
rm config.yml

date

