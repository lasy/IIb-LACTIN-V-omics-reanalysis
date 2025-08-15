from Bio import SeqIO

kept_recs = []
with open(snakemake.input[0]) as f:
    for record in SeqIO.parse(f, "fasta"):
        if len(record.seq) >= snakemake.params.min_contig_length:
            kept_recs.append(record)
            
with open(snakemake.output[0], "w") as f:
    SeqIO.write(kept_recs, f, "fasta")