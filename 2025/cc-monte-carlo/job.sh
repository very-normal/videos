#!/bin/bash

#SBATCH --job-name=YT-views-power-calc
#SBATCH --account=ACCOUNT_NUMBER
#SBATCH --mail-type END
#SBATCH --mail-user test@mail.com
#SBATCH --time=12:00:00
#SBATCH --partition=COMPUTE_PARTITION
#SBATCH --array=1-100
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
        
module load r/4.1.1

Rscript lvl3.R