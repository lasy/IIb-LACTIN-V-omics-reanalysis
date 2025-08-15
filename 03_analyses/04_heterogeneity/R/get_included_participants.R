
get_included_participants_initial_state <- function(mae, target_visit){
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(
      AVISITN %in% c(0, target_visit)
    ) %>% 
    group_by(USUBJID, ARM, SITENAME) %>% 
    summarize(has_all_visits = length(unique(AVISITN)) == 2, .groups = "drop") %>%
    filter(has_all_visits) %>% 
    select(-has_all_visits) %>% 
    arrange(USUBJID)
}


get_included_participants_success <- function(mae, target_visit){
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(
      AVISITN %in% c(0, 1, target_visit)
    ) %>% 
    select(Barcode, USUBJID, ARM, SITENAME, AVISITN) |> 
    left_join(tibble(Barcode = colnames(MultiAssayExperiment::assay(mae, "tax_16S")), has_16S = TRUE), by = join_by(Barcode)) |> 
    left_join(tibble(Barcode = colnames(MultiAssayExperiment::assay(mae, "cytokine_transformed")), has_cyt = TRUE), by = join_by(Barcode)) |> 
    group_by(USUBJID, ARM, SITENAME) %>% 
    summarize(
      n_visits = length(unique(AVISITN)), 
      n_16S = sum(has_16S, na.rm = TRUE), 
      n_cyt = sum(has_cyt, na.rm = TRUE), 
      .groups = "drop") |> 
    mutate(has_all_visits = ((n_16S == 3) & (n_cyt == 3))) |> 
    filter(has_all_visits) %>% 
    select(-has_all_visits, -starts_with("n_")) %>% 
    arrange(USUBJID)
}