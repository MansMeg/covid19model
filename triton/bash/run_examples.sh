
Rscript -e "remotes::install_local('rpackage', force = TRUE)"
export PBS_JOBID="base_refactor"
Rscript base_general_refactored.R base_general_speed --debug

ssh magnusm1@kosh.aalto.fi
ssh magnusm1@triton.aalto.fi

sbatch triton/run/covid19_base.sh
