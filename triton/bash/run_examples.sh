module load r/3.6.1-python3
Rscript -e "remotes::install_local('rpackage', force = TRUE)"
export PBS_JOBID="base_refactor"
Rscript base_model1_base.R base_general_speed --debug
Rscript base_model2.R base_general_speed --debug
Rscript base_model3.R base_general_speed --debug

bash triton/model_runs/model1_0410.sh

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
scp -r magnusm1@kosh.aalto.fi:/u/98/magnusm1/unix/Downloads/results '/Users/mansmagnusson/Dropbox (Personlig)/Projekt/covid19model'

# Copy two specific files
scp magnusm1@kosh.aalto.fi:/u/98/magnusm1/unix/Downloads/results/{base_general_speed-om1_ecdc-stanfit.Rdata,base_general_speed-om1_od-stanfit.Rdata} '/Users/mansmagnusson/Dropbox (Personlig)/Projekt/covid19model'
scp magnusm1@kosh.aalto.fi:/u/98/magnusm1/unix/Downloads/results/base_general_speed-om1_od-stanfit.Rdata '/Users/mansmagnusson/Dropbox (Personlig)/Projekt/covid19model'


