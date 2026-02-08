# Bacterial Genome Assembly Pipeline

A Snakemake workflow for bacterial genome assembly from paired-end Illumina reads.

## Pipeline Overview

This pipeline performs the following steps:
1. **Adapter trimming** (cutadapt)
2. **Quality filtering** (sickle)
3. **Genome assembly** (Unicycler with integrated SPAdes)
4. **Gene annotation** (Bakta)
5. **Taxonomic classification** (GTDB-Tk)
6. **Assembly statistics** (seqkit)
7. **Quality assessment** (CheckM2)

## Requirements

- Snakemake (>= 7.0)
- Conda/Mamba
- SLURM cluster (optional, for HPC execution)

## Setup

1. Clone this repository
2. Install required databases:
   - Bakta database: https://github.com/oschwengers/bakta#database
   - GTDB-Tk database: https://ecogenomics.github.io/GTDBTk/installing/index.html
   - CheckM2 database: https://github.com/chklovski/CheckM2

3. Update database paths in `config/config.yaml`:
   ```yaml
   bakta:
     db: "/path/to/bakta/db"
   gtdbtk:
     gtdb_data_path: "/path/to/gtdbtk/data"
   checkm2:
     database_path: "/path/to/checkm2/database/uniref100.KO.1.dmnd"
   ```

4. Create your sample sheet in `config/samples.csv`:
   ```csv
   isolate_id,fastq_1,fastq_2
   sample1,/path/to/sample1_R1.fastq.gz,/path/to/sample1_R2.fastq.gz
   sample2,/path/to/sample2_R1.fastq.gz,/path/to/sample2_R2.fastq.gz
   ```

## Usage

### Local execution
```bash
snakemake --use-conda --cores 8
```

### SLURM cluster execution
```bash
snakemake --use-conda --profile slurm
```

Where the SLURM profile should be configured according to your cluster specifications.

### Dry run
```bash
snakemake -n
```

### Generate workflow diagram
```bash
snakemake --dag | dot -Tpng > workflow.png
```

## Output

Results are organized in the `results/` directory:
- `cutadapt/`: Adapter-trimmed reads
- `sickle/`: Quality-filtered reads
- `unicycler/`: Genome assemblies
- `bakta/`: Gene annotations
- `gtdbtk/`: Taxonomic classifications
- `seqkit/`: Assembly statistics
- `checkm2/`: Quality assessment reports
- `summary/`: Combined summary tables

## Configuration

Edit `config/config.yaml` to adjust parameters for each tool.

## Resource Requirements

The pipeline is configured with SLURM resource allocations:
- Unicycler (with SPAdes): 64GB RAM, 24 CPUs
- GTDB-Tk: 128GB RAM, 32 CPUs
- CheckM2: 32GB RAM, 16 CPUs
- Bakta: 16GB RAM, 8 CPUs
- Other tools: 2-4GB RAM, 1-4 CPUs

Adjust these in the rule definitions as needed for your system.