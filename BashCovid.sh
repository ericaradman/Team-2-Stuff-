#!/bin/bash 
#SBATCH --reservation=teaching
#SBATCH --account=teaching
#SBATCH --partition=standard
#SBATCH --qos=normal
#SBATCH --job-name=myJobName  #optional name to give to your job 
#SBATCH -c 2 # Number of CPUS requested. If omitted, the default is 1 CPU. 
#SBATCH --mem=4G # Memory requested in megabytes. If omitted, the default is 1024 MB. 
#SBATCH --time=00:30:00 # Expected job run time (killed afterwards) default 3hrs.

Rscript dataManipulation.R
Rscript featureSelection.R
Rscript modelFitting&FeatureImoportance.R
Rscript Figures.R

