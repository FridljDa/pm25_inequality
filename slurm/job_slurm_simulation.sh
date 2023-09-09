#!/bin/bash

# Example of running R script with a job array

#SBATCH --job-name=flexible.sim.job
#SBATCH -A huber                # group to which you belong
#SBATCH -N 1                        # number of nodes
#SBATCH -n 3                        # number of cores
#SBATCH --mem 10G                    # memory pool for all cores
#SBATCH -t 1-2:00                   # runtime limit (D-HH:MM:SS)
#SBATCH -o simulation_flexible/out/flexible_dim_sim_out-%j.out
#SBATCH -e simulation_flexible/error_out/flexible_dim_sim_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address
# Load software
module load R

# Retrieve the command-line argument
num_splits=$1
split_index=$2

# Run R script
Rscript simulation_flexible/20230803_run_simulation_flexible_rule.R $num_splits $split_index