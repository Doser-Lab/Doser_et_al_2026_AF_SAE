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

bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03a -eo err.03a "Rscript 03a-main-spatial-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03b -eo err.03b "Rscript 03b-main-nonspatial-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03c -eo err.03c "Rscript 03c-main-spatial-noRE-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03d -eo err.03d "Rscript 03d-main-nonspatial-noRE-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03e -eo err.03e "Rscript 03e-main-spatial-only-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.03f -eo err.03f "Rscript 03f-main-nonspatial-only-stage-1.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04a -eo err.04a "Rscript 04a-main-spatial-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04b -eo err.04b "Rscript 04b-main-nonspatial-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04c -eo err.04c "Rscript 04c-main-spatial-noRE-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04d -eo err.04d "Rscript 04d-main-nonspatial-noRE-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04e -eo err.04e "Rscript 04e-main-spatial-only-stage-2.R"
bsub -n 1 -W 7200 -R span[hosts=1] -R "rusage[mem=15]" -q cnr -oo out.04f -eo err.04f "Rscript 04f-main-nonspatial-only-stage-2.R"
