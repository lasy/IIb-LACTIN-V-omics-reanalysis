rule download_checkm2_db:
    output:
        db=config["checkm2"]["database_path"]
    log:
        "logs/checkm2_db_download.log"
    conda:
        "../envs/checkm2.yaml"
    resources:
        slurm_partition="short",
        mem_mb=4000,
        cpus=4,
        runtime="12h"
    shell:
        """
        if [ ! -f {output.db} ]; then
            echo "CheckM2 database not found at {output.db}, downloading..." | tee {log}
            checkm2 database --download --path $(dirname {output.db}) >> {log} 2>&1
        else
            echo "CheckM2 database already exists at {output.db}" | tee {log}
        fi
        """

rule seqkit_stats:
    input:
        assembly="results/unicycler/{sample}/assembly.fasta"
    output:
        stats="results/seqkit/{sample}_stats.tsv"
    log:
        "logs/seqkit/{sample}.log"
    conda:
        "../envs/seqkit.yaml"
    resources:
        slurm_partition="short",
        mem_mb=2000,
        cpus=2,
        runtime="12h"
    shell:
        """
        seqkit stats -a -T {input.assembly} > {output.stats} 2> {log}
        """

rule checkm2:
    input:
        assembly="results/unicycler/{sample}/assembly.fasta",
        db=config["checkm2"]["database_path"]
    output:
        report="results/checkm2/{sample}/quality_report.tsv"
    params:
        input_dir="temp/checkm2/{sample}/input",
        output_dir="results/checkm2/{sample}",
        database_path=config["checkm2"]["database_path"],
        lowmem=config["checkm2"]["lowmem"]
    log:
        "logs/checkm2/{sample}.log"
    conda:
        "../envs/checkm2.yaml"
    threads:
        4
    resources:
        slurm_partition="short",
        mem_mb=32000,
        cpus_per_task=4,
        runtime="12h"
    shell:
        """
        mkdir -p {params.input_dir};
        cp {input.assembly} {params.input_dir}/{wildcards.sample}.fasta;
        sleep 10; 
        
        checkm2 predict \
            --input {params.input_dir} \
            -x ".fasta" \
            --output-directory {params.output_dir} \
            --database_path {params.database_path} \
            --threads {threads} \
            --force \
            > {log} 2>&1
        """