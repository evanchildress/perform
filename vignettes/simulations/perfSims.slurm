#!/bin/sh

#SBATCH -p normal
#SBATCH -A leetown
#SBATCH --output=%j-joboutput.out
#SBATCH -n 1
#SBATCH -c 3
#SBATCH -t 7-00:00:00
#SBATCH --array=1-200
#SBATCH --mail-user=echildress@usgs.gov
#SBATCH --mail-type=BEGIN,END,FAIL

module load jags/4.1.0-intel

Rscript ${SLURM_SUBMIT_DIR}/perform/runSims.R

exit
