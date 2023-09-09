#!/bin/bash

#SBATCH --job-name=00_parent

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

#SBATCH --mem 20G                    # memory pool for all cores
#SBATCH --partition=normal
#SBATCH --time=1-00:00:00

#SBATCH -o /share/pi/mkiang/dfridljand_air_pollution/pm25_inequality/slurm/out/pipeline_out-%j.out
#SBATCH -e /share/pi/mkiang/dfridljand_air_pollution/pm25_inequality/slurm/error_out/pipeline_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@gmail.com # send-to address

# Load software
year=$1

Rscript /share/pi/mkiang/dfridljand_air_pollution/pm25_inequality/pipeline/00_parent.R $year
