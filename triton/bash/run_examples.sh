module load r/3.6.1-python3
Rscript -e "remotes::install_local('rpackage', force = TRUE)"
export PBS_JOBID="base_refactor"
Rscript base_general_refactored_base.R base_general_speed --debug
Rscript base_general_refactored_oxford1_ecdc.R base_general_speed --debug
Rscript base_general_refactored_oxford2_ecdc.R base_general_speed --debug
Rscript base_general_refactored_oxford1_od.R base_general_speed --debug
Rscript base_general_refactored_oxford2_od.R base_general_speed --debug


slurm q

Rscript base_general_refactored_oxford1.R base_general_speed --debug

ssh magnusm1@kosh.aalto.fi
ssh magnusm1@triton.aalto.fi

sbatch triton/run/covid19_base.sh
sbatch triton/run/covid19_om1_od.sh
sbatch triton/run/covid19_om2_od.sh
sbatch triton/run/covid19_om1_ecdc.sh
sbatch triton/run/covid19_om2_ecdc.sh

# Install stuff
Rscript -e "install.packages('data.table')"
install.packages(c('rstan', 'data.table', 'lubridate', 'gdata', 'dplyr', 'tidyr', 'EnvStats', 'magrittr', 'optparse'))


scp username@b:/path/to/file /path/to/destination
scp <source> <destination>

# Copy results
scp -r magnusm1@triton.aalto.fi:/scratch/work/magnusm1/covid19model/results /u/98/magnusm1/unix/Downloads
scp -r magnusm1@kosh.aalto.fi:/u/98/magnusm1/unix/Downloads/results /Users/mansmagnusson/Dropbox (Personlig)/Projekt/covid19model

