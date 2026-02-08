rule bakta:
    input:
        assembly="results/unicycler/{sample}/assembly.fasta"
    output:
        gff="results/bakta/{sample}/{sample}.gff3",
        gbk="results/bakta/{sample}/{sample}.gbff",
        faa="results/bakta/{sample}/{sample}.faa",
        ffn="results/bakta/{sample}/{sample}.ffn",
        tsv="results/bakta/{sample}/{sample}.tsv",
        txt="results/bakta/{sample}/{sample}.txt"
    params:
        outdir="results/bakta/{sample}",
        db=config["bakta"]["db"],
        genus=config["bakta"]["genus"],
        species=config["bakta"]["species"],
        strain="{sample}",
        min_contig_length=config["bakta"]["min_contig_length"]
    log:
        "logs/bakta/{sample}.log"
    conda:
        "../envs/bakta.yaml"
    threads:
        8
    resources:
        slurm_partition="short",
        mem_mb=16000,
        cpus_per_task=8,
        runtime="12h"
    shell:
        """
        bakta \
            --db {params.db} \
            --output {params.outdir} \
            --prefix {wildcards.sample} \
            --force \
            --threads {threads} \
            --genus {params.genus} \
            --species {params.species} \
            --strain {params.strain} \
            --min-contig-length {params.min_contig_length} \
            {input.assembly} \
            > {log} 2>&1
        """


# rule download_gtdb:
#     output:
#         db=config["gtdbtk"]["database_path"]
#     log:
#         "logs/gtdb_db_download.log"
#     conda:
#         "../envs/gtdbtk.yaml"
#     resources:
#         slurm_partition="short",
#         mem_mb=4000,
#         cpus=4,
#         runtime="12h"
#     shell:
#         """
#         if [ ! -d {output.db} ]; then
#             echo "GTDB-Tk database not found at {output.db}, downloading..." | tee {log}
            
#             # Download to current directory
#             wget https://data.ace.uq.edu.au/public/gtdb/data/releases/latest/auxillary_files/gtdbtk_package/full_package/gtdbtk_data.tar.gz >> {log} 2>&1
            
#             # Extract to the target directory
#             mkdir -p $(dirname {output.db})
#             tar xvzf gtdbtk_data.tar.gz -C $(dirname {output.db}) >> {log} 2>&1
            
#             # Delete the downloaded tar.gz file
#             rm gtdbtk_data.tar.gz
            
#             echo "GTDB-Tk database download and extraction complete" | tee -a {log}
#         else
#             echo "GTDB-Tk database already exists at {output.db}" | tee {log}
#         fi
#         """

SAMPLES = samples_df["isolate_id"].tolist()


rule gtdbtk:
    input:
        assemblies=expand("results/unicycler/{sample}/assembly.fasta", sample=SAMPLES),
        db=config["gtdbtk"]["database_path"],
    output:
        summary="results/gtdbtk/gtdbtk.bac120.summary.tsv",
        tree="results/gtdbtk/gtdbtk.bac120.classify.tree",
        markers="results/gtdbtk/gtdbtk.bac120.markers_summary.tsv"
    params:
        genome_dir="results/gtdbtk/genomes",
        out_dir="results/gtdbtk",
        extension="fasta",
        prefix="gtdbtk",
        database_path=config["gtdbtk"]["database_path"],
        mash_db="resources/gtdbtk/mash_db",
        samples=SAMPLES
    log:
        "logs/gtdbtk/classify_all.log"
    conda:
        "../envs/gtdbtk.yaml"
    threads:
        16
    resources:
        slurm_partition="medium",
        mem_mb=250000,
        cpus_per_task=16,
        runtime="24h"
    shell:
        """
        mkdir -p {params.genome_dir}
        
        # Copy all assemblies to genome directory
        for sample in {params.samples}; do
            cp results/unicycler/${{sample}}/assembly.fasta {params.genome_dir}/${{sample}}.fasta
        done
        
        GTDBTK_DATA_PATH={params.database_path} gtdbtk classify_wf \
            --genome_dir {params.genome_dir} \
            --out_dir {params.out_dir} \
            --extension {params.extension} \
            --prefix {params.prefix} \
            --cpus {threads} \
            --pplacer_cpus 8 \
            --mash_db {params.mash_db} \
            > {log} 2>&1
        """