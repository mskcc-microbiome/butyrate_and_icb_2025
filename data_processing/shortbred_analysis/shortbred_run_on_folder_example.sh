#!/bin/bash

FOLDER="<replace me>"
OUTPUT_DIR="<replace me>"
PARAM_SOURCE="<replace me>"/butyrate_and_icb_2025/data_processing/shortbred_analysis/config_params.sh
THREADS=="<replace me (maybe 10?)>"

if [ ! -d $OUTPUT_DIR ]; then mkdir $OUTPUT_DIR; fi
if [ ! -d "$OUTPUT_DIR/logs" ]; then mkdir $OUTPUT_DIR/logs; fi
if [ ! -d "$OUTPUT_DIR/jobs" ]; then mkdir $OUTPUT_DIR/jobs; fi

for input in $FOLDER/*fastq.gz; do
    filebase=$(basename $input)
    sample="${filebase%%.fastq.gz}"
    output=$OUTPUT_DIR/${sample}_butyrate.txt
    tmp_dir=$OUTPUT_DIR/${sample}_tmp/
    JOB=$OUTPUT_DIR/jobs/${sample}_shortbred.sh
    output_log=$OUTPUT_DIR/logs/${sample}.out
    error_log=$OUTPUT_DIR/logs/${sample}.err
    job_name=shortbred_${sample}
    
    cp shortbred_template.sh $JOB
    sed -i -r "s|^(input=).*|\1${input}|" $JOB
    sed -i -r "s|^(output=).*|\1${output}|" $JOB
    sed -i -r "s|^(tmp_dir=).*|\1${tmp_dir}|" $JOB
    sed -i -r "s|^(param_source=).*|\1${PARAM_SOURCE}|" $JOB
    sed -i -r "s|^(threads=).*|\1${THREADS}|" $JOB

    
    sed -i -r "s|^(#BSUB -o .*).*|\1${output_log}|" $JOB
    sed -i -r "s|^(#BSUB -eo ).*|\1${error_log}|" $JOB
    sed -i -r "s|^(#BSUB -J ).*|\1${job_name}|" $JOB
    sed -i -r "s|^(#BSUB -n ).*|\1${THREADS}|" $JOB

    bsub < $JOB
done
