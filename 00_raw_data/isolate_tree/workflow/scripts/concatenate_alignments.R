library(tidyverse)

# Read the genome code mapping
mapping <- read_tsv(snakemake@input[["mapping"]], col_types = "cc")

# Get list of alignment files
alignment_files <- snakemake@input[["alignments"]]

# Function to read a FASTA file
read_fasta <- function(file) {
  lines <- readLines(file)
  headers <- lines[grepl("^>", lines)]
  sequences <- lines[!grepl("^>", lines)]

  # Group sequences by header
  header_indices <- which(grepl("^>", lines))

  result <- list()
  for (i in seq_along(header_indices)) {
    header <- lines[header_indices[i]]
    # Get sequence lines until next header or end
    if (i < length(header_indices)) {
      seq_lines <- lines[(header_indices[i] + 1):(header_indices[i + 1] - 1)]
    } else {
      seq_lines <- lines[(header_indices[i] + 1):length(lines)]
    }
    sequence <- paste(seq_lines, collapse = "")

    # Extract genome code (first 6 characters after >)
    genome_code <- str_sub(str_remove(header, "^>"), 1, 6)

    result[[genome_code]] <- sequence
  }

  return(result)
}

# Read all alignments
all_alignments <- list()
for (aln_file in alignment_files) {
  gene_seqs <- read_fasta(aln_file)
  all_alignments[[basename(aln_file)]] <- gene_seqs
}

# Get all unique genome codes across all alignments
all_genome_codes <- unique(unlist(lapply(all_alignments, names)))

# Build concatenated alignment for each genome
concatenated <- list()
for (genome_code in all_genome_codes) {
  concat_seq <- ""
  for (aln_name in names(all_alignments)) {
    gene_seqs <- all_alignments[[aln_name]]
    if (genome_code %in% names(gene_seqs)) {
      concat_seq <- paste0(concat_seq, gene_seqs[[genome_code]])
    } else {
      # If genome missing this gene, add gaps
      # Get length from first available sequence in this alignment
      gap_length <- nchar(gene_seqs[[1]])
      concat_seq <- paste0(concat_seq, paste(rep("-", gap_length), collapse = ""))
    }
  }
  concatenated[[genome_code]] <- concat_seq
}

# Map genome codes to isolate IDs
mapping_lookup <- setNames(mapping$isolate_id, mapping$genome_code)

# Write output FASTA with isolate IDs as headers
output_file <- snakemake@output[[1]]
writeLines("", output_file)  # Create/clear file

for (genome_code in names(concatenated)) {
  isolate_id <- mapping_lookup[genome_code]
  if (is.na(isolate_id)) {
    isolate_id <- genome_code  # Fallback if not in mapping
  }

  # Write header
  cat(paste0(">", isolate_id, "\n"), file = output_file, append = TRUE)

  # Write sequence (wrap at 80 characters for readability)
  sequence <- concatenated[[genome_code]]
  seq_wrapped <- gsub("(.{80})", "\\1\n", sequence)
  cat(seq_wrapped, "\n", file = output_file, append = TRUE, sep = "")
}
