#!/usr/bin/env python3
"""
Concatenate multiple gene alignments into a single super-alignment.
Renames sequences using isolate IDs instead of genome codes.
"""

from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord
import pandas as pd
from collections import defaultdict

# Read inputs from snakemake
alignment_files = snakemake.input["alignments"]
mapping_file = snakemake.input["mapping"]
output_file = snakemake.output[0]

# Read the genome code to isolate ID mapping
mapping_df = pd.read_csv(mapping_file, sep="\t")
code_to_isolate = dict(zip(mapping_df["genome_code"], mapping_df["isolate_id"]))

# Store sequences for each genome across all gene alignments
# Structure: {genome_code: {gene_file: sequence}}
genome_sequences = defaultdict(dict)
gene_lengths = {}  # Track alignment length for each gene

# Read all alignment files
for aln_file in alignment_files:
    gene_name = aln_file  # Store the file path as identifier
    sequences = SeqIO.parse(aln_file, "fasta")

    # Track alignment length (should be same for all sequences in this file)
    aln_length = None

    for record in sequences:
        # Extract genome code (first 6 characters of sequence ID)
        genome_code = str(record.id)[:6]

        # Store the sequence
        genome_sequences[genome_code][gene_name] = str(record.seq)

        # Record alignment length
        if aln_length is None:
            aln_length = len(record.seq)

    gene_lengths[gene_name] = aln_length

# Get all unique genome codes
all_genome_codes = sorted(genome_sequences.keys())

# Build concatenated sequences
concatenated_records = []

for genome_code in all_genome_codes:
    concat_seq = ""

    # For each gene alignment, add the sequence or gaps if missing
    for aln_file in alignment_files:
        if aln_file in genome_sequences[genome_code]:
            concat_seq += genome_sequences[genome_code][aln_file]
        else:
            # Genome is missing this gene - add gaps
            print(aln_file, genome_code)
            print(gene_lengths)
            concat_seq += "-" * gene_lengths[aln_file]

    # Map genome code to isolate ID
    isolate_id = code_to_isolate.get(genome_code, genome_code)

    # Create SeqRecord
    record = SeqRecord(
        Seq(concat_seq),
        id=isolate_id,
        description=""
    )
    concatenated_records.append(record)

# Write concatenated alignment
SeqIO.write(concatenated_records, output_file, "fasta")

print(f"Concatenated {len(alignment_files)} gene alignments")
print(f"Total alignment length: {len(concatenated_records[0].seq)} bp")
print(f"Number of genomes: {len(concatenated_records)}")
