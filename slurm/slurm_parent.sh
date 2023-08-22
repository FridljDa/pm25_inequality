#!/bin/bash

# of sbatch options.
#SBATCH --job-name=00_parent
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=normal
#SBATCH --time=0:01:00

# Load software
#module load R #
# Run R script
echo 'Hello World!'
