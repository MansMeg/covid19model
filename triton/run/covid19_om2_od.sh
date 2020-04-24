#!/bin/bash
#SBATCH --time=0-04:30:00
#SBATCH --cpus-per-task=4

# Defaults to directory of when submitted.  Explicit cd if needed (slurm defaults
# to submission directory).
# cd $WRKDIR/myproject/

# output goes into hello.out
# If you use srun for each command, the mem/cpu usage of each step
# can be seen individually with "slurm history"
module load r/3.6.1-python3
export PBS_JOBID="om2_od"
Rscript base_general_refactored_oxford2_od.R base_general_speed --full

