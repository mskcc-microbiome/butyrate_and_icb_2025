#!/bin/bash
#BSUB -J 
#BSUB -n 10
#BSUB -R span[ptile=10]
#BSUB -R rusage[mem=4]
#BSUB -W 40:00
#BSUB -o 
#BSUB -eo 

r1=unknown
r2=unknown
sample_name=unknown
threads=unknown
output=unknown
input=unknown
tmp_dir=unknown

SCRIPT_HOME=$(dirname "$(readlink -f "$0")")

if [ -f $SCRIPT_HOME/config_params.sh ]; then 
    source $SCRIPT_HOME/config_params.sh
else
    echo "$SCRIPT_HOME does not contain script config_params.sh"
fi

cat $r1 $r2 > $input

python $QUANTIFY_SCRIPT --markers $MARKER_FASTQ --search_program usearch \
            --usearch $USEARCH --threads $threads \
            --tmp $tmp_dir --wgs $input --results $output