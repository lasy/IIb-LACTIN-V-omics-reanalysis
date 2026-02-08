library(tidyverse)
genome_ids <- snakemake@params[["genome_ids"]]

n_genomes <- length(genome_ids)

hits <- read_table(
  snakemake@input[["hits"]],
  comment = "#",
  col_names = c(
    "target_name", "target_accession", "query_name", "query_accession",
    "evalue_full", "score_full", "bias_full",
    "evalue_best", "score_best", "bias_best",
    "exp", "reg", "clu", "ov", "env", "dom", "rep", "inc",
    "description"
  ),
  col_types = "ccccddddddiiiiiiic",
  n_max = Inf
)

mad_threshold <- 3

hits %>%
    mutate(genome_code = str_sub(target_name,1,6)) %>%
        group_by(query_name, genome_code) %>%
        arrange(evalue_full) %>%
        filter(row_number() == 1) %>% # get best hit
        ungroup() %>%
        group_by(query_name) %>%
        mutate(
          log_evalue = log10(evalue_full),
          med = median(log_evalue),
          mad = mad(log_evalue, constant = 1.4826)
        ) %>%
        filter(log_evalue <= med + mad_threshold * mad) %>%
        ungroup() %>%
        group_by(query_name) %>%
        filter(n() >= snakemake@params[["min_prevalence"]]*n_genomes) %>% # only keep genes with prevalence past threshold
        ungroup() %>%
        select(query_name, target_name) %>%
        write_csv(snakemake@output[["parsed_hits"]])

