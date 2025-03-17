**Note `butyrate_pw_genes_aa_seqs.fa` is the fasta file we used to build the ShortBRED marker db. We also provide you with the pre-built marker db: `butyrate_pw_genes_markers.fa`**

## Analysis setup:

### Install dependencies:
***(Note that we primarily use the SSH links for `git clone`.  The HTTPS git clone commands are also provided in paranthesis)***

- Clone this repo to a machine with your raw data where you plan to do your analysis. 
    - `git clone git@github.com:mskcc-microbiome/butyrate_and_icb_2025.git` (`git clone https://github.com/mskcc-microbiome/butyrate_and_icb_2025.git`)
- Install [usearch](https://www.drive5.com/usearch/download.html) (just need v6.0.307 or later, we use v 11.0.667):
    - `curl https://www.drive5.com/downloads/usearch11.0.667_i86linux32.gz --output usearch11.0.667_i86linux32.gz`
    - `gunzip usearch11.0.667_i86linux32.gz`
- Install [Shortbred](https://huttenhower.sph.harvard.edu/shortbred/)
    - `git clone git@github.com:biobakery/shortbred.git` (`git clone https://github.com/biobakery/shortbred.git`)


## Analysis Options:

### Bash script:

#### Set up Environment:

First create and activate an environment with the correct installed packages:

From the base dir of this repo:

```
mamba env create -n shortbred_env -f data_processing/shortbred_analysis/envs/shortbred_env.yaml 
```
```
mamba activate shortbred_env
```
Make sure this env is activated whenever you run this shortbred analysis! 

#### Update Config

Now open `data_processing/shortbred_analysis/config_params.sh` and edit the following variables:

- `USEARCH` - this should be the path to usearch that you installed in Analysis setup
- `MARKER_FASTQ` - this is the marker fastq we supplied for this analysis!  Location: `data_processing/shortbred_analysis/butyrate_pw_genes_markers.fa`
- `QUANTIFY_SCRIPT` -  the path to the `shortbred_quantify.py` script in your Shortbred installation. 

#### (recommended) Test Run

Save `data_processing/shortbred_analysis/shortbred_template.sh` with a new name ie `cp data_processing/shortbred_analysis/shortbred_template.sh data_processing/shortbred_analysis/shortbred_test_run.sh`.

Edit the header to add job specific details (note we use LSF headers, please update to whatever submission manager you use!)

- `#BSUB -J` -> `#BSUB -J test_shortbred_run`
- `#BSUB -o` -> `#BSUB -o test_shortbred_run.stdout`
- `#BSUB -eo` -> `#BSUB -eo test_shortbred_run.stderr`
- `#BSUB -n` -> `#BSUB -n 1` (for example, can do more than this too, and set the `threads` parameter below to match.)

Manually edit the following parameters:

- `r1`: path to r1 direction `fastq.gz` file for testing. 
- `r2`: path to r2 direction `fastq.gz` file for testing. 
- `threads`: treads used in test (can default to 1 if needed).
- `output`: desired output file.  ie: {$sample_name}_butyrate.txt".
- `input`: a file for the concatenated `r1` and `r2` files (when submitting the job array we make this a temporary file.)
- `tmp_dir`: name of a temporary directory this analysis can use while running. (ie {$sample_name}_shortbred_tmp)

### Snakemake:

Using the Snakefile in this folder you should be able to run this file locally or on an HPC. If you use LSF we have a [Snakemake profile](https://github.com/vdblab/vdblab-profile) you can use for submissions. 

ShortBRED has the following environmental requirements:
- Python 2.7.9
- Biopython v1.65
- ncbi-blast-2.2.28+
- usearch v6.0.307 (Please make sure this is up to date. Earlier versions of usearch use a different command for making database than what is expected by ShortBRED.)
- MUSCLE v3.8.31
- CD-HIT version 4.6

These environmental requirements are captured in the `shortbred_env.yaml` file used by the `Snakefile` for `ShortBRED` analysis. 

Our analysis currently assumes a structure where each sample has a directory containing R1 and R2 as `fastq.gz` files.  The `sample_dir` and `sample` name will be provided as dynamic entries to the config.

You will also need to update the config file for the snakemake workflow for the following parameters:
- `quantify_script`: (the path to the shortBRED quantify_script)
- `marker_fastq`: (path to the marker fastq we provided (or updated marker.fastq if using your own reference)).
- `usearch`: the path to your usearch installation. 

