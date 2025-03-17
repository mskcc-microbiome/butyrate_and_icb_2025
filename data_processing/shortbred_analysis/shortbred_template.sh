#!/bin/bash
#BSUB -J 
#BSUB -n 
#BSUB -R rusage[mem=4]
#BSUB -W 24:00
#BSUB -o 
#BSUB -eo 

input=unknown
output=unknown
tmp_dir=unknown
param_source=unknown
threads=1

source $param_source


python $QUANTIFY_SCRIPT --markers $MARKER_FASTQ --search_program usearch \
            --usearch $USEARCH --threads $threads \
            --tmp $tmp_dir --wgs $input --results $output