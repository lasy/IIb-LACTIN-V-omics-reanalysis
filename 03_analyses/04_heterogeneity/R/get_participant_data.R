
get_demographics <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  clin <- MultiAssayExperiment::colData(mae) %>% as.data.frame()
  
  variables <- c("Barcode", "RACEGR2", "AGE", "EDULVL", "N_PAST_BV")

  participant_data <- 
    clin %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(all_of(variables)) %>%  
    dplyr::rename(Age = AGE, `Education level` = EDULVL, `N past BV` = N_PAST_BV) %>% 
    distinct() %>% 
    arrange(Barcode) 
  
  # `mbpsl` from the `ade4` package can only handle numerical variables. 
  # For categorical data (such as race), we use one-hot encoding
  participant_data_dummy <- 
    participant_data %>% 
    as_tibble() %>%
    mutate(
      `N past BV` = 
        forcats::fct_relevel(`N past BV`, "Unknown",  after = 2L) %>% as.integer(),
      `Education level` = `Education level` %>% as.integer()
    ) %>% 
    arrange(RACEGR2) %>% 
    mutate(tmp = 1) %>% 
    tidyr::pivot_wider(
      names_from = RACEGR2, values_from = tmp, values_fill = 0,
      names_prefix = "Race: "
    ) 
  
  participant_data_dummy <- 
    participant_data_dummy  %>% 
    as.data.frame() %>% 
    arrange(Barcode) %>% 
    column_to_rownames(var = "Barcode") 
  
  list(
    participant_data = participant_data, 
    participant_data_dummy = participant_data_dummy
  )
  
}

get_participant_data <- function(mae, IDs, include_site = FALSE){
  
  clin <- MultiAssayExperiment::colData(mae) %>% as.data.frame()
  
  variables <- c("USUBJID", "RACEGR2", "AGE", "EDULVL", "N_PAST_BV")
  if (include_site) variables <- c(variables, "SITENAME")
  
  participant_data <- 
    clin %>% 
    filter(USUBJID %in% IDs) %>% 
    select(all_of(variables)) %>%  
    rename(Age = AGE, `Education level` = EDULVL, `N past BV` = N_PAST_BV) %>% 
    distinct() %>% 
    arrange(USUBJID) 
  
  # `mbpsl` from the `ade4` package can only handle numerical variables. 
  # For categorical data (such as race), we use one-hot encoding
  participant_data_dummy <- 
    participant_data %>% 
    as_tibble() %>%
    mutate(
      `N past BV` = 
        forcats::fct_relevel(`N past BV`, "Unknown",  after = 2L) %>% as.integer(),
      `Education level` = `Education level` %>% as.integer()
    ) %>% 
    arrange(RACEGR2) %>% 
    mutate(tmp = 1) %>% 
    tidyr::pivot_wider(
      names_from = RACEGR2, values_from = tmp, values_fill = 0,
      names_prefix = "Race: "
    ) 

  if (include_site) {
    participant_data_dummy <- 
      participant_data_dummy %>%
      arrange(SITENAME) %>%
      mutate(tmp = 1) %>%
      tidyr::pivot_wider(
        names_from = SITENAME, values_from = tmp, values_fill = 0,
        names_prefix = "Site: "
      )
  }
  
  participant_data_dummy <- 
    participant_data_dummy %>% 
    arrange(USUBJID) %>% 
    select(-USUBJID)  %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
  
  list(
    participant_data = participant_data, 
    participant_data_dummy = participant_data_dummy
  )
}
