localrules: target, combine_runs

import pandas as pd
from collections import defaultdict

psomagen_samples = pd.read_csv("input/new_psomagen_paths.csv", index_col=False)
bmc_samples = pd.read_csv("input/bmc_novaseq_paths.csv", index_col=False)

sample_runs = {}

for record in psomagen_samples.to_dict(orient='records'):
    if record['sample_id'] in sample_runs:
        sample_runs[record['sample_id']] = sample_runs[record['sample_id']] + [record['run_id']]
    else:
        sample_runs[record['sample_id']] = [record['run_id']]

for record in bmc_samples.to_dict(orient='records'):
    if record['lactinv_sample_id'] in sample_runs:
        sample_runs[record['lactinv_sample_id']] = sample_runs[record['lactinv_sample_id']] + [record['run_id']]
    else:
        sample_runs[record['lactinv_sample_id']] = [record['run_id']]


def get_bmc_bams(wildcards):
    # /n/groups/kwon/data1/sequencing_run_archive_DO_NOT_EDIT/2023_02_02_NovaSeq/230202YilA/D23-115193-2-6110F/230202YilA_D23-115193-2_phiX_bestmap.bam
    bmc_id = [r for r in bmc_samples.to_dict(orient="records") if r['lactinv_sample_id'] == wildcards.sample_id][0]["bmc_sample_id"]
    bam_1 = "/n/groups/kwon/data1/sequencing_run_archive_DO_NOT_EDIT/2023_02_02_NovaSeq/230202YilA/{}-1-6110F/230202YilA_{}-1_phiX_bestmap.bam".format(bmc_id, bmc_id)
    bam_2 = "/n/groups/kwon/data1/sequencing_run_archive_DO_NOT_EDIT/2023_02_02_NovaSeq/230202YilA/{}-2-6110F/230202YilA_{}-2_phiX_bestmap.bam".format(bmc_id, bmc_id)
    return {"bam_1":bam_1, "bam_2":bam_2}


ALL_SAMPLES = list(sample_runs.keys())

# Define the path to the T2T reference genome
T2T_REF = '/n/groups/kwon/joseph/dbs/combined_T2T_CRCh38_reference_for_host_filtering.fna'

# Define the basename for the Bowtie2 index
HISAT2_INDEX_PREFIX = '/n/groups/kwon/joseph/dbs/combined_T2T_CRCh38_reference_for_host_filtering_hisat2'

rule target:
    input:
        expand('results/{sample_id}/combined/{sample_id}_1.final.fastq.gz', sample_id=ALL_SAMPLES),
        expand('results/{sample_id}/combined/{sample_id}_2.final.fastq.gz', sample_id=ALL_SAMPLES),
        #expand("lactinv_gtpro_tsvs/temp_06_combine_filtered_reads_{sample_id}__gtpro__crisp.tsv.temp", sample_id=ALL_SAMPLES)
        "output/strainfacts/na_lactinv_202410121.world.mgen.filt.fit.ra.tsv"
        #'results/multiqc_report.html'


rule prepare_bmc_reads:
    input:
        unpack(get_bmc_bams)
    output:
        forward_reads_1 = "temp/00.prepare_bmc_reads/{run_id}--lane_1_{sample_id}_1.fastq.gz",
        forward_reads_2 = "temp/00.prepare_bmc_reads/{run_id}--lane_2_{sample_id}_1.fastq.gz",
        reverse_reads_1 = "temp/00.prepare_bmc_reads/{run_id}--lane_1_{sample_id}_2.fastq.gz",
        reverse_reads_2 = "temp/00.prepare_bmc_reads/{run_id}--lane_2_{sample_id}_2.fastq.gz",
        forward_reads_concat = "temp/00.prepare_bmc_reads/{run_id}--{sample_id}_1.fastq.gz",
        reverse_reads_concat = "temp/00.prepare_bmc_reads/{run_id}--{sample_id}_2.fastq.gz"
    conda:
        "envs/picard.yaml"
    retries: 3
    resources:
        cpus_per_task = 1,
        mem_mb=lambda wc, attempt: 16000 + 16000*attempt,
        runtime = "12h",
        partition = "short"
    shell:
        """
        picard SamToFastq -I {input.bam_1} -F {output.forward_reads_1} -F2 {output.reverse_reads_1};
        picard SamToFastq -I {input.bam_2} -F {output.forward_reads_2} -F2 {output.reverse_reads_2};
        cat {output.forward_reads_1} {output.forward_reads_2} > {output.forward_reads_concat};
        cat {output.reverse_reads_1} {output.reverse_reads_2} > {output.reverse_reads_concat};
        """

def get_raw_reads_for_sample(wildcards):
    forward_reads = ""
    reverse_reads = ""
    if wildcards.run_id == "230202YilA": #bmc run with bams
        forward_reads = "temp/00.prepare_bmc_reads/{}--{}_1.fastq.gz".format(wildcards.run_id, wildcards.sample_id)
        reverse_reads = "temp/00.prepare_bmc_reads/{}--{}_2.fastq.gz".format(wildcards.run_id, wildcards.sample_id)
    else:
        matching_records = [r for r in psomagen_samples.to_dict(orient="records") if (r['sample_id'] == wildcards.sample_id) and (r['run_id'] == wildcards.run_id)]
        if len(matching_records) != 1:
            raise
        forward_reads = matching_records[0]['forward']
        reverse_reads = matching_records[0]['reverse']
    return {"forward_reads":forward_reads, "reverse_reads":reverse_reads}


rule initial_fastqc:
    input:
        unpack(get_raw_reads_for_sample)
    output:
        forward_fastqc='results/{sample_id}/fastqc/{sample_id}_{run_id}_1_fastqc.zip',
        reverse_fastqc='results/{sample_id}/fastqc/{sample_id}_{run_id}_2_fastqc.zip'
    threads:
        4
    resources:
        cpus_per_task = 4,
        mem_mb = 4000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/fastqc.yaml"
    shell:
        """
        mkdir -p results/{wildcards.sample_id}/fastqc/
        fastqc -t {threads} -o results/{wildcards.sample_id}/fastqc/ {input.forward_reads} {input.reverse_reads}
        """

rule remove_adapters:
    input:
        unpack(get_raw_reads_for_sample),
    output:
        forward_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1.trimmed.fastq.gz',
        reverse_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2.trimmed.fastq.gz',
        forward_report='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1_cutadapt_report.json',
        reverse_report='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2_cutadapt_report.json'
    threads:
        4
    resources:
        cpus_per_task = 4,
        mem_mb = 4000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/cutadapt.yaml"
    shell:
        """
        cutadapt -a CTGTCTCTTAT --cores={threads} \
            -o {output.forward_reads} \
            --json={output.forward_report} \
            {input.forward_reads}
        cutadapt -a CTGTCTCTTAT --cores={threads} \
            -o {output.reverse_reads} \
            --json={output.reverse_report} \
            {input.reverse_reads}
        """

rule filter_quality:
    input:
        forward_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1.trimmed.fastq.gz',
        reverse_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2.trimmed.fastq.gz'
    output:
        forward_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1.filtered.fastq.gz',
        reverse_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2.filtered.fastq.gz',
        unpaired='results/{sample_id}/{run_id}/{sample_id}_{run_id}_unpaired.fastq.gz',
        log='results/{sample_id}/{run_id}/{sample_id}_{run_id}_sickle.log'
    params:
        qual=20,
        readlen=50
    resources:
        mem_mb = 4000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/sickle.yaml"
    shell:
        """
        sickle pe -f {input.forward_reads} -r {input.reverse_reads} \
            -t sanger \
            -o {output.forward_reads} -p {output.reverse_reads} \
            -s {output.unpaired} \
            -g -q {params.qual} -l {params.readlen} -x -n \
            2> {output.log}
        """

rule bbduk_remove_human_fast:
    input:
        forward_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1.filtered.fastq.gz',
        reverse_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2.filtered.fastq.gz'
    output:
        forward_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_1.bbduk.fastq.gz',
        reverse_reads='results/{sample_id}/{run_id}/{sample_id}_{run_id}_2.bbduk.fastq.gz',
        stats='results/{sample_id}/{run_id}/{sample_id}_{run_id}_bbduk_stats.txt'
    params:
        ref='/n/groups/kwon/joseph/dbs/combined_T2T_CRCh38_reference_for_host_filtering.fna',  # Update with the path to T2T reference genome FASTA
        k=31,
        hdist=0
    threads: 8
    resources:
        cpus_per_task = 8,
        mem_mb = 100000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/bbmap.yaml"
    shell:
        """
        bbduk.sh in1={input.forward_reads} in2={input.reverse_reads} \
            out1={output.forward_reads} out2={output.reverse_reads} \
            ref={params.ref} -Xmx85g\
            k={params.k} hdist={params.hdist} \
            stats={output.stats} \
            threads={threads}
        """


rule build_hisat2_index:
    input:
        T2T_REF
    output:
        idx_files=touch(expand('{prefix}.index_done_flag', prefix=HISAT2_INDEX_PREFIX))
    threads: 4
    resources:
        cpus_per_task = 4,
        mem_mb = 32000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/hisat2.yaml"
    shell:
        """
        hisat2-build -p {threads} {input} {HISAT2_INDEX_PREFIX}
        """

rule hisat2_remove_human_sensitive:
    input:
        forward_reads='results/{sample}/{run}/{sample}_{run}_1.bbduk.fastq.gz',
        reverse_reads='results/{sample}/{run}/{sample}_{run}_2.bbduk.fastq.gz',
        idx_files=rules.build_hisat2_index.output.idx_files
    output:
        forward_reads='results/{sample}/{run}/{sample}_{run}_1.clean.fastq.gz',
        reverse_reads='results/{sample}/{run}/{sample}_{run}_2.clean.fastq.gz',
        report='results/{sample}/{run}/{sample}_{run}_hisat2_report.txt'
    params:
        hisat2_index=HISAT2_INDEX_PREFIX
    threads: 4
    resources:
        cpus_per_task = 4,
        mem_mb = 24000,
        runtime = "12h",
        partition = "short"
    conda:
        "envs/hisat2.yaml"
    shell:
        """
        hisat2 -x {params.hisat2_index} -1 {input.forward_reads} -2 {input.reverse_reads} \
            -p {threads} --no-spliced-alignment \
            --score-min L,0,-0.6 \
            --un-conc-gz results/{wildcards.sample}/{wildcards.run}/{wildcards.sample}_{wildcards.run}_%.clean.fastq.gz \
            -S /dev/null 2> {output.report}
        """

rule combine_runs:
    input:
        forward_reads=expand('results/{{sample_id}}/{run_id}/{{sample_id}}_{run_id}_1.clean.fastq.gz', run_id=lambda wildcards: sample_runs[wildcards.sample_id]),
        reverse_reads=expand('results/{{sample_id}}/{run_id}/{{sample_id}}_{run_id}_2.clean.fastq.gz', run_id=lambda wildcards: sample_runs[wildcards.sample_id])
    output:
        forward_reads='results/{sample_id}/combined/{sample_id}_1.final.fastq.gz',
        reverse_reads='results/{sample_id}/combined/{sample_id}_2.final.fastq.gz'
    shell:
        """
        cat {input.forward_reads} > {output.forward_reads}
        cat {input.reverse_reads} > {output.reverse_reads}
        """


rule run_metagenotype:
    input:
        forward_reads='results/{sample_id}/combined/{sample_id}_1.final.fastq.gz',
        reverse_reads='results/{sample_id}/combined/{sample_id}_2.final.fastq.gz'
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
        fit = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.fit"
    conda:
        "envs/strainfacts.yaml"
    params:
        hyperparams = "gamma_hyper=1e-15 pi_hyper=0.1 pi_hyper2=0.1 rho_hyper=1.0 rho_hyper2=1.0",
        anneal_hyperparams = "gamma_hyper=0.999 --anneal-steps 120000",
        n_strains = 300,
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
        fit = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.fit"
    output:
        rel_abund = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.fit.ra.tsv",
        genotypes = "output/strainfacts/na_lactinv_202410121.world.mgen.filt.fit.geno.tsv"
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
