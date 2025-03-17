#!/bin/bash

FOLDER=
THREADS=10

while read line; do 

    echo "Submitting pipeline job for ${sample_name}"
    cp shortbred_template.sh ${JOB_DIR}shortbred_${sample_name}.sh

    sed -i -r "s|^(input=).*|\1${sample_name}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(output=).*|\1${values[1]}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(tmp_dir=).*|\1${values[2]}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(threads=).*|\1${THREADS}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(param_source=).*|\1${values[3]}|" ${JOB_DIR}shortbred_${sample_name}.sh

    sed -i -r "s|^(#BSUB -o .*).*|\1${output_log}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(#BSUB -eo ).*|\1${error_log}|" ${JOB_DIR}shortbred_${sample_name}.sh
    sed -i -r "s|^(#BSUB -J ).*|\1${jobname}|" ${JOB_DIR}shortbred_${sample_name}.sh
    
    sed -i -r "s|^(#BSUB -n ).*|\1${THREADS}|" ${JOB_DIR}shortbred_${sample_name}.sh
    bsub < ${JOB_DIR}shortbred_${sample_name}.sh

done <  ${sample_location_file}
