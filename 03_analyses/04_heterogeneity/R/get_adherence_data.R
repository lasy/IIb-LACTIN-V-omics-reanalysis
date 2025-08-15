

get_adherence_since_LPIPV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, AVISITN, DAY, N_MISSED_DOSES, LAST_DOSE) %>% 
    group_by(AVISITN) %>% 
    mutate(
      `Nb of missed doses` = N_MISSED_DOSES, # |> replace_na(0),
      `Days since last dose` = LAST_DOSE %>% replace_na(max(DAY) + 5) 
    ) %>% 
    ungroup() %>% 
    select(Barcode,  `Nb of missed doses`, `Days since last dose`) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}


get_adherence_throughout <-function(mae, Barcodes, target_visit){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  A_colnames <- c("DOSES_W1","DOSES_W2_4","DOSES_W5_8","DOSES_W9_12")
  if (target_visit == 7) A_colnames <- c(A_colnames, "DOSES_W13_24")
  
  res <- 
    MultiAssayExperiment::colData(mae) |> 
    as.data.frame() |> 
    filter(Barcode %in% Barcodes) |> 
    arrange(Barcode) |> 
    select(all_of(A_colnames))
  
  colnames(res) <- 
    colnames(res) |> str_replace_all("DOSES_", "N doses ") |> str_replace_all("_", " to ")
    
  res
  
}

# Deprecated


get_adherence_data <- function(data_dir, IDs) {
  
  events <- 
    readRDS(
      file = str_c(data_dir, "01_preprocessed/Clinical metadata/events.RDS")
    )
  
  events %>% 
    filter(
      USUBJID %in% IDs,
      VARIABLE == "Dose"
    ) %>% 
    mutate(
      week_group  = 
        case_when(
          DAY %in% c(1:7) ~ "W1",
          DAY %in% c(8:28) ~ "W2-4",
          DAY %in% c(29:56) ~ "W5-8", 
          DAY %in% c(57:84) ~ "W9-12",
        )
    ) %>% 
    group_by(USUBJID, week_group) %>% 
    summarize(n_doses = n(), .groups = "drop") %>% 
    tidyr::pivot_wider(
      id_cols = USUBJID,
      names_from = week_group,
      values_from = n_doses,
      names_prefix = "N doses "
    ) %>% 
    mutate(
      across(.cols = starts_with("N doses"), function(x) tidyr::replace_na(x, 0))
    ) %>% 
    arrange(USUBJID)  %>% 
    select(-USUBJID) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
  
}

get_adherence_since_LV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, AVISITN, N_DOSES_LV, LAST_DOSE) %>% 
    group_by(AVISITN) %>% 
    mutate(
      # TODO: change here so that it takes into account the time since LV for each participant.
      max_n_doses = case_when(AVISITN == 2 ~ 11, AVISITN %in% 3:4 ~ 8, TRUE ~ 1), # max(N_DOSES_LV, na.rm = TRUE),
      N_DOSES_LV = N_DOSES_LV %>% replace_na(0),
      `% doses taken` = 100*N_DOSES_LV/max_n_doses,
      `% doses taken` = case_when(is.infinite(`% doses taken`) ~ 100, TRUE ~ `% doses taken`),
      `Days since last dose` = LAST_DOSE %>% replace_na(max(LAST_DOSE, na.rm = TRUE) + 1) 
    ) %>% 
    ungroup() %>% 
    select(Barcode, `% doses taken`, `Days since last dose`) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}

