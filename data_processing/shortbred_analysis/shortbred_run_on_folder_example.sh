#!/bin/bash

FOLDER=

while read line; do 

    IFS=",";values=($line);IFS='***';

    sample_name="${values[0]}"
    output_log="${LOG_DIR}caz_pipeline_${sample_name}.stdout"
    error_log="${LOG_DIR}caz_pipeline_${sample_name}.stderr"
    jobname=pipeline_${sample_name}

    prokka_sample_dir=${PROKKA_DIR}${sample_name}/
    dbcan_sample_dir=${DBCAN_DIR}${sample_name}/
    if [ ! -d $dbcan_sample_dir ]; then

        echo "Submitting pipeline job for ${sample_name}"
        cp run_pipeline.sh ${JOB_DIR}caz_pipeline_${sample_name}.sh

        sed -i -r "s|^(sample_name=).*|\1${sample_name}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(r1=).*|\1${values[1]}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(r2=).*|\1${values[2]}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(assembly=).*|\1${values[3]}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh

        sed -i -r "s|^(prokka_dir=).*|\1${prokka_sample_dir}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(dbcan_dir=).*|\1${dbcan_sample_dir}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh

        sed -i -r "s|^(#BSUB -o .*).*|\1${output_log}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(#BSUB -eo ).*|\1${error_log}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        sed -i -r "s|^(#BSUB -J ).*|\1${jobname}|" ${JOB_DIR}caz_pipeline_${sample_name}.sh
        bsub < ${JOB_DIR}caz_pipeline_${sample_name}.sh
    else
        echo "dbcan dir for ${sample_name} already exists, skipping."
    fi

   
done <  ${sample_location_file}