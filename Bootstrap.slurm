#!/bin/bash
#SBATCH --job-name=Bootstrap
#SBATCH --time=300:00
#SBATCH --mail-type=NONE
#SBATCH --mem=1g
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000
#SBATCH --partition=standard
#SBATCH --account=biostat625f23_class
module load Rtidyverse
# Set the R script name
R_SCRIPT="Bootstrap.R"
# Run the R script with SLURM_ARRAY_TASK_ID as an argument
R CMD BATCH --no-save --no-restore ${R_SCRIPT} ${R_SCRIPT%.*}_$SLURM_ARRAY_TASK_ID.Rout



