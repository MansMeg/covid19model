#!/bin/bash
#SBATCH --time=0-01:00:00
#SBATCH --cpus-per-task=4

module load r/3.6.1-python3

# Setup yml config file
cat > config.yml <<EOF

job_id: "model1_debug"

model_arguments:
  stan_model: "base_general_speed.stan"
  model_formula: "~ S1"
  N2: 90
  seed: 4711  
  date:
    start: "2020-01-01"
    end: "2020-04-10"

data:
  countries: ["Denmark", "Italy", "Germany", "Spain", "United_Kingdom", "France", "Norway", "Belgium", "Austria", "Sweden", "Switzerland", "Greece", "Portugal", "Netherlands", "Finland"]
  daily_data: "odv4"

stan_arguments:
  iter: 20
  warmup: 10
  chains: 1
  thin: 1
  control:
    adapt_delta: 0.8
    max_treedepth: 10

EOF

# Run model
Rscript run_model.R config.yml

# Remove config
rm config.yml

