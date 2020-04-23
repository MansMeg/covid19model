#!/bin/bash
#SBATCH --time=0-23:00:00    # 5 mins
#SBATCH --cpus-per-task=12

# Defaults to directory of when submitted.  Explicit cd if needed (slurm defaults
# to submission directory).
# cd $WRKDIR/myproject/

# output goes into hello.out
# If you use srun for each command, the mem/cpu usage of each step
# can be seen individually with "slurm history"
export PBS_JOBID="base"
Rscript base_general_refactored.R base_general_speed --full
