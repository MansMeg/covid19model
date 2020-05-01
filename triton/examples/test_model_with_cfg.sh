#!/bin/bash
#SBATCH --time=0-01:00:00
#SBATCH --cpus-per-task=4

module load r/3.6.1-python3
Rscript run_model.R triton/configs/test_model.yml

