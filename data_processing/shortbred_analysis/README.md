Using the Snakefile in this folder you should be able to run this file locally or on an HPC. 

ShortBRED has the following environmental requirements:
- Python 2.7.9
- Biopython v1.65
- ncbi-blast-2.2.28+
- usearch v6.0.307 (Please make sure this is up to date. Earlier versions of usearch use a different command for making database than what is expected by ShortBRED.)
- MUSCLE v3.8.31
- CD-HIT version 4.6

These environmental requirements are captured in the `shortbred_env.yaml` file used by the `Snakefile` for `ShortBRED` analysis. 

You will also need to update the config file for the snakemake workflow for the following parameters:
- `quantify_script`: (the path to the shortBRED quantify_script)
- `marker_fastq`: (path to the marker fastq we provided (or updated marker.fastq if using your own reference)).
- `usearch`: the path to your usearch installation. 

Note `butyrate_pw_genes_aa_seqs.fa` is the fasta file we used to build the ShortBRED marker db. We also provide you with the pre-built marker db: `butyrate_pw_genes_markers.fa`