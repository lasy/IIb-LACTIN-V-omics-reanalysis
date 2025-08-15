library(tidyverse)


data_dir <- "temp/raw_luminex"

files_to_process <- str_c(data_dir, list.files(data_dir) %>% keep(str_detect, pattern = "\\.csv"))

sample_manual_choices <- read_csv(snakemake@input[["sample_manual_choices"]]) # If samples were run more than once, manually pick which run to use in this file


#' parse_luminex
#'
#' teturns a dataframe where each datatype in the luminex table is parsed into its own dataframe
#'
#' @param lines the value from read_lines on the raw luminex output
#'
#' @noRd
parse_luminex <- function(lines) {
  result_lines <- split_results_from_luminex_file(lines)
  batch_info <- batch_info_from_luminex_file(lines)
  tables <- parse_result_lines(result_lines)
  dplyr::bind_rows(
    tibble::tibble(datatype = "batch_metadata", dataset = list(batch_info)),
    dplyr::select(tables, datatype, dataset),
  )
}

#' split_results_from_luminex_file
#'
#' returns the lines after the "Results" and before the "-- CRC --" and removes empty lines
#'
#' @param lines the value from read_lines on the raw luminex output
#'
#' @noRd
split_results_from_luminex_file <- function(lines) {
  result_index <- which(lines == "\"Results\"") + 2
  crc_index <- which(lines == "-- CRC --") - 2
  print(crc_index)
  lines[result_index:crc_index] |>
    purrr::keep(~ !is.na(.x) & .x != "") |>
    unlist()
}

#' batch_info_from_luminex_file
#'
#' returns metadata present in the top of the file.
#'
#' @param lines the value from read_lines on the raw luminex output
#'
#' @noRd
batch_info_from_luminex_file <- function(lines) {
  header_index <- which(str_detect(lines, "\"Most Recent Calibration and Verification Results:\""))
  header_info <- tibble::tibble(raw = lines[1:header_index - 1]) %>%
    dplyr::mutate(raw = str_replace_all(raw, "\"", "")) %>%
    dplyr::mutate(
      field = purrr::map_chr(raw, ~ stringr::str_split_fixed(.x, ",", n = 2) %>% purrr::pluck(1)),
      value = purrr::map_chr(raw, ~ stringr::str_split_fixed(.x, ",", n = 2) %>% purrr::pluck(2))
    ) %>%
    filter(raw != "") %>%
    mutate(value = str_replace_all(value, ",", " "))
  standard_lot_info_index <- which(str_detect(lines, "^\"AssayLotInfo:\""))
  standard_lot_info <- readr::read_csv(
    str_c(lines[(standard_lot_info_index + 1):(standard_lot_info_index + 2)], collapse = "\n"),
    col_types = cols(.default = col_character())
  ) %>%
    rename_with(~ str_c("Standard_", .x)) %>%
    pivot_longer(everything(), names_to = "field", values_to = "value")
  bind_rows(header_info, standard_lot_info) %>% select(-raw)
}

#' parse_result_lines
#'
#' returns a dataframe where each row is a result type and the data is stored in a list column
#'
#' @param result_lines the value from split_results_from_luminex_file
#' @noRd
parse_result_lines <- function(result_lines) {
  tibble::tibble(data_type_index = which(stringr::str_detect(result_lines, "DataType"))) |>
    dplyr::mutate(dataset_start = data_type_index + 1, dataset_end = dplyr::lead(data_type_index - 1)) |>
    tidyr::replace_na(list(dataset_end = length(result_lines))) |>
    dplyr::mutate(datatype = purrr::map_chr(data_type_index, ~ pluck(result_lines, .x) |> # this mutate extracts and reformats the dataset name
      stringr::str_split(",", simplify = TRUE) |>
      purrr::pluck(2) |>
      stringr::str_replace_all("[\"\\^/%]", "") |>
      stringr::str_trim() |>
      stringr::str_replace_all(" - ", "_") |>
      stringr::str_replace_all(" ", "_") |>
      tolower())) |>
    filter(dataset_start != dataset_end) |>
    dplyr::mutate(dataset = purrr::map2(
      dataset_start, dataset_end,
      ~ readr::read_csv(
        str_c(result_lines[.x:.y], collapse = "\n"),
        col_types = cols(.default = col_character())
      )
    ))
}

parsed_runs <- map_dfr(files_to_process, ~ parse_luminex(read_lines(.x)) %>% mutate(run_id = .x))
save.image("debug.Rdata")

samples_plate_map <- parsed_runs %>%
  filter(datatype == "count") %>%
  unnest(dataset) %>%
  pivot_longer(cols = c(-Location, -Sample, -datatype, -run_id), names_to = "analyte", values_to = "count") %>%
  select(Sample, run_id) %>%
  filter(str_detect(Sample, "^[0-9]{9}$")) %>%
  anti_join(select(sample_manual_choices, Sample)) %>%
  bind_rows(sample_manual_choices)

count_table <- parsed_runs %>%
  filter(datatype == "count") %>%
  unnest(dataset) %>%
  pivot_longer(cols = c(-Location, -Sample, -datatype, -run_id), names_to = "analyte", values_to = "count") %>%
  mutate(count = as.numeric(count)) %>%
  semi_join(samples_plate_map)

net_mfi_table <- parsed_runs %>%
  filter(datatype == "net_mfi") %>%
  unnest(dataset) %>%
  pivot_longer(cols = c(-Location, -Sample, -datatype, -run_id), names_to = "analyte", values_to = "net_mfi") %>%
  semi_join(samples_plate_map)

concentration_table <- parsed_runs %>%
  filter(datatype == "result") %>%
  unnest(dataset) %>%
  pivot_longer(cols = c(-Location, -Sample, -datatype, -run_id), names_to = "analyte", values_to = "concentration") %>%
  semi_join(samples_plate_map)

expected_standard_concentrations <- parsed_runs %>%
  filter(datatype == "standard_expected_concentration") %>%
  pull(dataset) %>%
  pluck(1) %>%
  pivot_longer(-Reagent, values_to = "concentration", names_to = "analyte") %>%
  mutate(concentration = as.numeric(concentration))

write_csv(count_table, snakemake@output[["count_table"]])
write_csv(net_mfi_table, snakemake@output[["net_mfi_table"]])
write_csv(concentration_table, snakemake@output[["concentration_table"]])
write_csv(expected_standard_concentrations, snakemake@output[["standards_table"]])
