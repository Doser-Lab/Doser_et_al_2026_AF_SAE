#!/bin/bash
#BSUB -n 5
#BSUB -W 7200
#BSUB -J pred 
#BSUB -o stdout.%J
#BSUB -e stderr.%J
#BSUB -R span[hosts=1]
#BSUB -R "rusage[mem=30]"
#BSUB -q cnr
module load openmpi-gcc
module load R
Rscript 06a-predict.R

