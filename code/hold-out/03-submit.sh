#!/bin/bash
#
# Script:  05a-submit.sh
# Usage: Quick script to submit multiple jobs to NCSU HPC. 
# Author: Jeffrey W. Doser (adapted from NCSU HPC)
#
## To run, type:
#     ./05a-submit.sh 
#  Script must have execute permissions, i.e.,
#     chmod u+x 05a-submit.sh

module load openmpi-gcc
module load R

bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01a -eo err.01a "Rscript 01a-ho-random-main-spatial-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01b -eo err.01b "Rscript 01b-ho-random-main-nonspatial-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01c -eo err.01c "Rscript 01c-ho-random-main-spatial-noRE-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01d -eo err.01d "Rscript 01d-ho-random-main-nonspatial-noRE-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01e -eo err.01e "Rscript 01e-ho-random-main-spatial-only-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.01f -eo err.01f "Rscript 01f-ho-random-main-nonspatial-only-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02a -eo err.02a "Rscript 02a-ho-random-main-spatial-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02b -eo err.02b "Rscript 02b-ho-random-main-nonspatial-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02c -eo err.02c "Rscript 02c-ho-random-main-spatial-noRE-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02d -eo err.02d "Rscript 02d-ho-random-main-nonspatial-noRE-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02e -eo err.02e "Rscript 02e-ho-random-main-spatial-only-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.02f -eo err.02f "Rscript 02f-ho-random-main-nonspatial-only-stage-2.R"
