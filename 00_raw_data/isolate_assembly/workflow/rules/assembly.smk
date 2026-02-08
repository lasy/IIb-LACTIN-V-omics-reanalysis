rule unicycler:
    input:
        r1="results/fastp/{sample}_1.trimmed.fastq.gz",
        r2="results/fastp/{sample}_2.trimmed.fastq.gz"
    output:
        assembly="results/unicycler/{sample}/assembly.fasta",
        gfa="results/unicycler/{sample}/assembly.gfa",
        log_file="results/unicycler/{sample}/unicycler.log"
    params:
        outdir="results/unicycler/{sample}",
        mode=config["unicycler"]["mode"],
        min_fasta_length=config["unicycler"]["min_fasta_length"],
        kmers=config["unicycler"]["kmers"],
        keep=config["unicycler"]["keep"],
    log:
        "logs/unicycler/{sample}.log"
    conda:
        "../envs/unicycler.yaml"
    resources:
        slurm_partition="short",
        mem_mb=64000,
        cpus=24,
        runtime="12h"
    shell:
        """
        unicycler \
            -1 {input.r1} \
            -2 {input.r2} \
            -o {params.outdir} \
            -t {resources.cpus} \
            --mode {params.mode} \
            --min_fasta_length {params.min_fasta_length} \
            --kmers {params.kmers} \
            --keep {params.keep} \
            > {log} 2>&1
        """