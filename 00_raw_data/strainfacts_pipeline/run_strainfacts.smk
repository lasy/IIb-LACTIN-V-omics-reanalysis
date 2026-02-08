localrules: target, combine_runs

import pandas as pd
from collections import defaultdict

reads_path = "/n/groups/kwon/joseph/data/na_lactinv_host_filtered_reads/hisat2_t2t_filtered_20241022/"

ALL_SAMPLES = glob_wildcards("/n/groups/kwon/joseph/data/na_lactinv_host_filtered_reads/hisat2_t2t_filtered_20241022/{sample_id}_1.final.fastq.gz").sample_id
problem_samples = ["202307084","202306127","202305468","202306548","202309037","202306488","202309617","202307128","202302704","202308721","202302744"]
ALL_SAMPLES = [s for s in ALL_SAMPLES if s not in problem_samples]

rule target:
    input:
        strains = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.n100.fit.ra.tsv"

rule run_metagenotype:
    input:
        forward_reads="/n/groups/kwon/joseph/data/na_lactinv_host_filtered_reads/hisat2_t2t_filtered_20241022/{sample_id}_1.final.fastq.gz",
        reverse_reads="/n/groups/kwon/joseph/data/na_lactinv_host_filtered_reads/hisat2_t2t_filtered_20241022/{sample_id}_2.final.fastq.gz"
    output:
        concat_reads = temp("temp/06.combine_filtered_reads/{sample_id}.fastq"),
        gtpro_out = "all_gtpro_tsvs/temp_06_combine_filtered_reads_{sample_id}__gtpro__crisp.tsv"
    conda:
        "envs/gtpro.yaml"
    threads:
        4
    retries:
        3
    resources:
        cpus_per_task = 4,
        mem_mb = 24000,
        runtime = "3h",
        partition = "gpu --gres=gpu:1"
    shell:
        "cat {input.forward_reads} {input.reverse_reads} > {output.concat_reads}.gz;gunzip {output.concat_reads}.gz;"
        "module load gcc/9.2.0; ./gt-pro/GT_Pro genotype -d output/gtpro/crisp/crisp.bin {output.concat_reads} -t {threads};"
        "mv temp_06_combine_filtered_reads_{wildcards.sample_id}__gtpro__crisp.tsv all_gtpro_tsvs/;"

rule gtpro_parse:
    input:
        forward_tsv = "all_gtpro_tsvs/temp_06_combine_filtered_reads_{sample_id}__gtpro__crisp.tsv"
    output:
        forward_tsv = temp("lactinv_gtpro_tsvs/temp_06_combine_filtered_reads_{sample_id}__gtpro__crisp.tsv.temp")
    conda:
        "envs/gtpro.yaml"
    resources:
        mem_mb = 8000,
        runtime = "1h",
        partition = "short"
    shell:
        "./gt-pro/GT_Pro parse --dict output/gtpro/crisp/crisp.snp_dict.tsv --in {input.forward_tsv} --out {output.forward_tsv};"

rule gtpro_merge:
    input:
        tsvs = expand("lactinv_gtpro_tsvs/temp_06_combine_filtered_reads_{sample_id}__gtpro__crisp.tsv.temp", sample_id=ALL_SAMPLES)
    output:
        merged_tsv = "output/gtpro_genotypes/all_merged.tsv"
    conda:
        "envs/tidyverse.yaml"
    resources:
        mem_mb = 24000,
        runtime = "12h",
        partition = "short"
    script:
        "scripts/merge_gtpro.R"

rule run_strainfacts:
    input:
        merged_tsv = "output/gtpro_genotypes/all_merged.tsv"
    output:
        world_mgen = "output/strainfacts/na_lactinv_202410121.world.mgen"
    conda:
        "envs/strainfacts.yaml"
    resources:
        mem_mb = 24000,
        runtime = "12h",
        partition = "short"
    shell:
        "sfacts load --gtpro-metagenotype {input.merged_tsv} {output.world_mgen}"


rule strainfacts_filter:
    input:
        world_mgen = "output/strainfacts/na_lactinv_202410121.world.mgen"
    output:
        filtered_mgen = "output/strainfacts/na_lactinv_202410121.world.filt.mgen"
    conda:
        "envs/strainfacts.yaml"
    resources:
        mem_mb = 96000,
        runtime = "12h",
        partition = "short"
    shell:
        """
        sfacts filter_mgen \
        --verbose \
        --min-minor-allele-freq 0 \
        --min-horizontal-cvrg 0.1 \
        {input.world_mgen} {output.filtered_mgen}
        """


rule strainfacts_fit:
    input:
        filtered_mgen = "output/strainfacts/na_lactinv_202410121.world.filt.mgen"
    output:
        fit = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.n100.fit"
    conda:
        "envs/strainfacts.yaml"
    params:
        hyperparams = "gamma_hyper=1e-15 pi_hyper=0.1 pi_hyper2=0.1 rho_hyper=1.0 rho_hyper2=1.0",
        anneal_hyperparams = "gamma_hyper=0.999 --anneal-steps 120000",
        n_strains = 100,
    resources:
        cpus_per_task = 1,
        mem_mb = 100000,
        runtime = "12h",
        partition = "gpu --gres=gpu:1"
    shell:
        """
        sfacts fit \
        --verbose --hyperparameters {params.hyperparams} \
        --anneal-hyperparameters {params.anneal_hyperparams} \
        --num-strains {params.n_strains} --device cuda \
        --random-seed 0 \
        {input.filtered_mgen} {output.fit}
        """

rule strainfacts_dump:
    input:
        fit = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.n100.fit"
    output:
        rel_abund = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.n100.fit.ra.tsv",
        genotypes = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.n100.fit.geno.tsv"
    conda:
        "envs/strainfacts.yaml"
    resources:
        cpus_per_task = 1,
        mem_mb = 96000,
        runtime = "12h",
        partition = "short"
    shell:
        "sfacts dump {input.fit} --community {output.rel_abund} --genotype {output.genotypes}"

def gather_multiqc_inputs(wildcards):
    inputs = []
    for sample in ALL_SAMPLES:
        runs = sample_runs[sample]
        # FastQC reports
        inputs.extend([f'results/{sample}/fastqc/{sample}_{run}_1_fastqc.zip' for run in runs])
        inputs.extend([f'results/{sample}/fastqc/{sample}_{run}_2_fastqc.zip' for run in runs])
        # Cutadapt reports
        inputs.extend([f'results/{sample}/{run}/{sample}_{run}_1_cutadapt_report.json' for run in runs])
        inputs.extend([f'results/{sample}/{run}/{sample}_{run}_2_cutadapt_report.json' for run in runs])
        # Sickle logs
        inputs.extend([f'results/{sample}/{run}/{sample}_{run}_sickle.log' for run in runs])
        # BBduk stats
        inputs.extend([f'results/{sample}/{run}/{sample}_{run}_bbduk_stats.txt' for run in runs])
        # HISAT2 reports
        inputs.extend([f'results/{sample}/{run}/{sample}_{run}_hisat2_report.txt' for run in runs])
    return inputs

rule multiqc:
    input:
        gather_multiqc_inputs
    output:
        'results/multiqc_report.html'
    threads: 4
    conda:
        "envs/multiqc.yaml"
    shell:
        """
        multiqc -o results/ results/
        """
