#!/bin/bash
#BSUB -J test
#BSUB -n 10
#BSUB -R rusage[mem=4]
#BSUB -W 24:00
#BSUB -o /data/peledj/baichom1/Projects/test_shortbred_analysis/test.o
#BSUB -eo /data/peledj/baichom1/Projects/test_shortbred_analysis/test.e

input=unknown
output=unknown
tmp_dir=unknown
threads=1
param_source=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo $param_source
source $param_source/config_params.sh
echo $QUANTIFY_SCRIPT
echo "hi"

#python $QUANTIFY_SCRIPT --markers $MARKER_FASTQ --search_program usearch \
#            --usearch $USEARCH --threads $threads \
#            --tmp $tmp_dir --wgs $input --results $output