
rule fastp:
    """Adapter trimming and quality filtering using fastp."""
    input:
        sample = lambda wildcards: [
            samples_df[samples_df["isolate_id"] == wildcards.sample].iloc[0]["fastq_1"],
            samples_df[samples_df["isolate_id"] == wildcards.sample].iloc[0]["fastq_2"]
        ]
    output:
        trimmed = [
            "results/fastp/{sample}_1.trimmed.fastq.gz",
            "results/fastp/{sample}_2.trimmed.fastq.gz"
        ],
        unpaired = "results/fastp/{sample}.unpaired.fastq.gz",
        json = "results/fastp/{sample}.fastp.json",
        html = "results/fastp/{sample}.fastp.html"
    log:
        "results/logs/fastp/{sample}_fastp.log"
    params:
        adapters = lambda wildcards: (
            f"--adapter_sequence {config['fastp']['adapter_r1']}" if config['fastp']['adapter_r1'] else ""
        ) + (
            f" --adapter_sequence_r2 {config['fastp']['adapter_r2']}" if config['fastp']['adapter_r2'] else ""
        ),
        extra = lambda wildcards:
        (
            " --trim_poly_g" if config["fastp"]["trim_poly_g"] else ""
        ) + (
            " --trim_poly_x" if config["fastp"]["trim_poly_x"] else ""
        ) + (
            f" --complexity_threshold {config['fastp']['complexity_threshold']}"
            if config["fastp"]["complexity_threshold"] else ""
        ) + (
            f" --qualified_quality_phred {config['fastp']['qualified_quality_phred']}"
        ) + (
            f" --length_required {config['fastp']['length_required']}"
        )
    threads: 4
    resources:
        cpus_per_task=4,
        mem_mb=8000,
        runtime="4h",
        slurm_partition="short"
    wrapper:
        "v7.5.0/bio/fastp"