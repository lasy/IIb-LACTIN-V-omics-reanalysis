library(tidyverse)
library(tidymodels)

count_table <- read_csv(snakemake@input[["count_table"]])
net_mfi_table <- read_csv(snakemake@input[["net_mfi_table"]])
concentration_table <- read_csv(snakemake@input[["concentration_table"]])
expected_standard_concentrations <- read_csv(snakemake@input[["standards_table"]])

standard_extents <- expected_standard_concentrations %>%
    mutate(concentration = as.numeric(concentration)) %>%
    pivot_wider(names_from = Reagent, values_from = concentration) %>%
    select(analyte, min_standard = Standard1, max_standard = Standard7)

parsed_concentration_table <- concentration_table %>%
    filter(analyte != "Total Events") %>%
    left_join(standard_extents) %>%
    mutate(parsed_concentration_1 = str_replace_all(concentration, "[ <>]", "")) %>%
    mutate(parsed_concentration_1 = as.numeric(parsed_concentration_1)) %>%
    mutate(censored = case_when(
        parsed_concentration_1 <= min_standard ~ "below_detection_limit",
        parsed_concentration_1 >= max_standard ~ "above_detect_limit",
        TRUE ~ "not_censored"
    )) %>%
    mutate(lod_concentration_2 = case_when(
        censored == "below_detection_limit" ~ min_standard * 0.5, # if below detection limit, set to half of the minimum standard
        censored == "above_detect_limit" ~ max_standard * 1, # if above detection limit, set to the maximum standard
        TRUE ~ parsed_concentration_1 # if not censored, set to the parsed concentration
    )) %>%
    left_join(select(count_table, Location, Sample, run_id, analyte, count)) %>%
    mutate(low_bead_concentration_3 = if_else(count <= 0, NA_real_, lod_concentration_2)) # if there are less than N beads, set the concentration to NA

# keep analytes only with less than 25% of the samples with missing or censored values
kept_analytes <- parsed_concentration_table %>%
    group_by(analyte) %>%
    filter((sum(is.na(low_bead_concentration_3)) + sum(!is.na(low_bead_concentration_3) & censored == "below_detection_limit") + sum(!is.na(low_bead_concentration_3) & censored == "above_detect_limit")) <= 0.8 * n()) %>%
    ungroup() %>%
    select(analyte) %>%
    distinct()

parsed_concentration_table %>%
    semi_join(kept_analytes) %>%
    write_csv(snakemake@output[["parsed_concentration"]])
