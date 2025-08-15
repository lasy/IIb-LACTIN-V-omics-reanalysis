library(tidyverse)
library(SummarizedExperiment)

parsed_concentration_table <- read_csv(snakemake@input[["parsed_concentration"]])
parsed_concentration_wide <- parsed_concentration_table %>%
    select(Sample, analyte, low_bead_concentration_3) %>%
    pivot_wider(names_from = "analyte", values_from = "low_bead_concentration_3")

transpose_temp <- parsed_concentration_wide %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("analyte")

colnames(transpose_temp) <- as.character(transpose_temp[1, ])

table_for_se <- as_tibble(transpose_temp) %>%
    dplyr::slice(-1) %>%
    column_to_rownames("Sample")

saveRDS(SummarizedExperiment(assays = list("luminex" = table_for_se)), snakemake@output[["summarised_experiment"]])
