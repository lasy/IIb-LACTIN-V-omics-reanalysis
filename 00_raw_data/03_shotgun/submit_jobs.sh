#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=12:00:00
#SBATCH --mem=16GB
#SBATCH --partition=priority
#SBATCH --job-name=submit_jobs

mkdir -p ./slurm

snakemake -s all_runs.smk --use-conda --conda-prefix=~/snakemake_conda_prefix --cluster-config cluster.yaml \
  --cluster "sbatch --output=slurm/{rulename}.{jobid} -p {cluster.partition} -N {cluster.nodes} -n {cluster.cores} --mem={cluster.mem} --time={cluster.time}" \
  --jobname {rulename}.{jobid} --jobs 500 --keep-going --rerun-incomplete --groups trimgalore=group_trimgalore fastq_to_bam=group_fastq_to_bam filter_host_and_quality=group_filter_host_and_quality sam_to_fastq_paired=group_sam_to_fastq_paired combine_sample_reads=group_combine_sample_reads \
 --group-components group_trimgalore=25 group_fastq_to_bam=25 group_filter_host_and_quality=25 group_sam_to_fastq_paired=25 group_combine_sample_reads=25
