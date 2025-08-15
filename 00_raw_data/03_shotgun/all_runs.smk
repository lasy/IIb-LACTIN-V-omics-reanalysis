import pandas as pd
from collections import defaultdict

psomagen_samples = pd.read_csv("input/psomagen_paths.csv", index_col=False, dtype=defaultdict(str))
bmc_samples = pd.read_csv("input/bmc_novaseq_paths.csv", index_col=False, dtype=defaultdict(str))

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
    bmc_id = [r for r in bmc_samples.to_dict(orient="records") if r['lactinv_sample_id'] == wildcards.sample][0]["bmc_sample_id"]
    bam_1 = "/n/groups/kwon/data1/sequencing_run_archive_DO_NOT_EDIT/2023_02_02_NovaSeq/230202YilA/{}-1-6110F/230202YilA_{}-1_phiX_bestmap.bam".format(bmc_id, bmc_id)
    bam_2 = "/n/groups/kwon/data1/sequencing_run_archive_DO_NOT_EDIT/2023_02_02_NovaSeq/230202YilA/{}-2-6110F/230202YilA_{}-2_phiX_bestmap.bam".format(bmc_id, bmc_id)
    return {"bam_1":bam_1, "bam_2":bam_2}


rule target:
    input:
        expand("output/breseq_crispatus/03.filter_host/{sample}", sample = list(sample_runs.keys()))

rule prepare_bmc_reads:
    input:
        unpack(get_bmc_bams)
    output:
        forward_reads_1 = temp("temp/00.prepare_bmc_reads/{run_id}.lane_1_{sample}_1.fastq.gz"),
        forward_reads_2 = temp("temp/00.prepare_bmc_reads/{run_id}.lane_2_{sample}_1.fastq.gz"),
        reverse_reads_1 = temp("temp/00.prepare_bmc_reads/{run_id}.lane_1_{sample}_2.fastq.gz"),
        reverse_reads_2 = temp("temp/00.prepare_bmc_reads/{run_id}.lane_2_{sample}_2.fastq.gz"),
        forward_reads_concat = temp("temp/00.prepare_bmc_reads/{run_id}.{sample}_1.fastq.gz"),
        reverse_reads_concat = temp("temp/00.prepare_bmc_reads/{run_id}.{sample}_2.fastq.gz")
    conda:
        "envs/picard.yaml"
    shell:
        """
        picard SamToFastq -I {input.bam_1} -F {output.forward_reads_1} -F2 {output.reverse_reads_1};
        picard SamToFastq -I {input.bam_2} -F {output.forward_reads_2} -F2 {output.reverse_reads_2};
        cat {output.forward_reads_1} {output.forward_reads_2} > {output.forward_reads_concat};
        cat {output.reverse_reads_1} {output.reverse_reads_2} > {output.reverse_reads_concat};
        """


def get_reads_for_trimgalore(wildcards):
    forward_reads = ""
    reverse_reads = ""
    if wildcards.run_id == "230202YilA": #bmc run with bams
        forward_reads = "temp/00.prepare_bmc_reads/{}.{}_1.fastq.gz".format(wildcards.run_id, wildcards.sample)
        reverse_reads = "temp/00.prepare_bmc_reads/{}.{}_2.fastq.gz".format(wildcards.run_id, wildcards.sample)
    else:
        matching_records = [r for r in psomagen_samples.to_dict(orient="records") if (r['sample_id'] == wildcards.sample) and (r['run_id'] == wildcards.run_id)]
        if len(matching_records) != 1:
            raise
        forward_reads = matching_records[0]['forward']
        reverse_reads = matching_records[0]['reverse']
    return {"forward_reads":forward_reads, "reverse_reads":reverse_reads}


rule trimgalore:
    input:
        unpack(get_reads_for_trimgalore)
    output:
        temp("temp/01.trim_galore/{run_id}/{run_id}.{sample}_R1_val_1.fq.gz"),
        temp("temp/01.trim_galore/{run_id}/{run_id}.{sample}_R2_val_2.fq.gz")
    conda:
        "envs/trimgalore.yaml"
    params:
        length = 70,
        stringency = 1,
        error_rate = 0.05,
        max_n = 1
    shell:
        """
        trim_galore -e {params.error_rate} --retain_unpaired \\
        --clip_R1 1 --three_prime_clip_R1 1  \\
        --length {params.length} --stringency {params.stringency} \\
        --max_n {params.max_n} --phred33 \\
        --output_dir temp/01.trim_galore/{wildcards.run_id} \\
        --basename {wildcards.run_id}.{wildcards.sample} \\
        --paired {input.forward_reads} {input.reverse_reads};
        """

rule fastq_to_bam:
    input:
        forward_reads = "temp/01.trim_galore/{run_id}/{run_id}.{sample}_R1_val_1.fq.gz",
        reverse_reads = "temp/01.trim_galore/{run_id}/{run_id}.{sample}_R2_val_2.fq.gz"
    output:
        bam = temp("temp/02.fastqtosam/{run_id}.{sample}.bam")
    conda:
        "envs/picard.yaml"
    shell:
        """
        picard FastqToSam F1={input.forward_reads} F2={input.reverse_reads} O={output.bam} RG=A SM={wildcards.sample}
        """

rule filter_host_and_quality:
    input:
        reads_bam = "temp/02.fastqtosam/{run_id}.{sample}.bam",
        host_kmers = "/n/groups/kwon/joseph/dbs/gatk_tutorial/pathseq_host.bfi",
        host_img = "/n/groups/kwon/joseph/dbs/gatk_tutorial/pathseq_host.fa.img"
    output:
        paired_bam = "temp/03.filter_host/{run_id}.{sample}_paired.bam",
        unpaired_bam = "temp/03.filter_host/{run_id}.{sample}_unpaired.bam",
        metrics = "temp/03.filter_host/{run_id}.{sample}_metrics.txt",
    conda:
        "envs/gatk.yaml"
    threads:
        8
    shell:
        """
        module load java/jdk-1.8u112; gatk PathSeqFilterSpark  \
        --input {input.reads_bam} \
        --paired-output {output.paired_bam} \
        --unpaired-output {output.unpaired_bam} \
        --min-clipped-read-length 60 \
        --kmer-file {input.host_kmers} \
        --filter-bwa-image {input.host_img} \
        --filter-metrics {output.metrics} \
        --bam-partition-size 4000000
        """

rule sam_to_fastq_paired:
    input:
        input_bam = "temp/03.filter_host/{run_id}.{sample}_paired.bam"
    output:
        forward_reads = "temp/04.back_to_fastq/03.filter_host/{run_id}.{sample}.f.fq",
        reverse_reads = "temp/04.back_to_fastq/03.filter_host/{run_id}.{sample}.r.fq",
        unpaired_reads = "temp/04.back_to_fastq/03.filter_host/{run_id}.{sample}.unpaired.fq"
    conda:
        "envs/picard.yaml"
    shell:
        """
        picard SamToFastq -I {input.input_bam} -F {output.forward_reads} -F2 {output.reverse_reads} -FU {output.unpaired_reads}
        """

def get_all_filtered_reads(wildcards):
    return {"forward_reads": ["temp/04.back_to_fastq/03.filter_host/{}.{}.f.fq".format(run_id, wildcards.sample) for run_id in sample_runs[wildcards.sample]],
    "reverse_reads": ["temp/04.back_to_fastq/03.filter_host/{}.{}.r.fq".format(run_id, wildcards.sample) for run_id in sample_runs[wildcards.sample]]}

rule combine_sample_reads:
    input:
        unpack(get_all_filtered_reads)
    output:
        forward_reads = "temp/06.combine_filtered_reads/{sample}.f.fq",
        reverse_reads = "temp/06.combine_filtered_reads/{sample}.r.fq"
    shell:
        "cat {input.forward_reads} > {output.forward_reads}; cat {input.reverse_reads} > {output.reverse_reads};"

rule breseq:
    input:
        forward_reads = "temp/06.combine_filtered_reads/{sample}.f.fq",
        reverse_reads = "temp/06.combine_filtered_reads/{sample}.r.fq",
        reference = "input/consensus_polished.fasta"
    output:
        directory("output/breseq_crispatus/03.filter_host/{sample}")
    conda:
        "envs/breseq.yaml"
    threads:
        1
    shadow:
        "shallow"
    shell:
        "breseq -n {wildcards.sample} -o /n/scratch3/users/j/je112/breseq_temp_output_{wildcards.sample}/ --brief-html-output --polymorphism-prediction --no-junction-prediction --brief-html-output -r {input.reference} {input.forward_reads} {input.reverse_reads} --num-processors {threads} && cp /n/scratch3/users/j/je112/breseq_temp_output_{wildcards.sample} output/breseq/03.filter_host/{wildcards.sample}"
