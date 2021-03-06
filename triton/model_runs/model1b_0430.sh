#!/bin/bash
#SBATCH --time=0-08:00:00
#SBATCH --cpus-per-task=4

date

git rev-parse --short HEAD

export JOB_ID="model1b_0430"

module load r/3.6.1-python3

# Setup yml config file
cat > $JOB_ID.yml <<EOF

job_id: "$JOB_ID"

model_arguments:
  stan_model: "base.stan"
  model_formula: "~ schools...universities + self.isolating.if.ill + public.events + any.intervention + lockdown + social.distancing.encouraged"
  N2: 110
  seed: 4711
  date:
    start: "2020-01-01"
    end: "2020-04-30"

data:
  countries: ["Denmark", "Italy", "Germany", "Spain", "United_Kingdom", "France", "Norway", "Belgium", "Austria", "Sweden", "Switzerland", "Greece", "Portugal", "Netherlands", "Finland"]
  daily_data: "icv3"

stan_arguments:
  iter: 3000
  warmup: 2000
  chains: 4
  thin: 2
  control:
    adapt_delta: 0.995
    max_treedepth: 15

EOF

# Run model
Rscript run_model.R $JOB_ID.yml

# Remove config
rm $JOB_ID.yml

date
