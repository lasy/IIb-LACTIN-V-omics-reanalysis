

get_birth_control_since_LPIPV <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, BC, BC_CHANGE) %>% 
    mutate(tmp_BC = 1) %>% 
    pivot_wider(names_from = BC, values_from = tmp_BC, values_fill = 0) %>%   
    mutate(`Changed BC since last PIPV` = (BC_CHANGE*1) |> replace_na(0)) |> 
    select(-BC_CHANGE) %>% 
    as.data.frame() %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}

get_birth_control_throughout <- function(mae, Barcodes, target_visit){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  if (target_visit == 4) BC_colname <- "BC_W1_12" else BC_colname <- "BC_W1_24"
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, all_of(BC_colname)) %>% 
    mutate(tmp_BC = 1) %>% 
    pivot_wider(names_from = all_of(BC_colname), values_from = tmp_BC, values_fill = 0) %>%   
    as.data.frame() %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}





# Deprecated



get_birth_control <- function(clin, IDs, target_visit){
  clin %>% 
    filter(
      USUBJID %in% IDs,
      AVISITN <= target_visit
      ) %>% 
    select(USUBJID, AVISITN, BC, BC_CHANGE) %>% 
    arrange(USUBJID, AVISITN) %>% 
    group_by(USUBJID) %>% 
    summarize(
      n_BC = length(unique(BC)),
      BC = str_c(BC %>% sort() %>%  unique(), collapse = ", ")
    ) %>% 
    mutate(BC = ifelse((n_BC != 1) | str_detect(BC, ","), "mix or change", BC)) %>%
    select(-n_BC) %>% 
    mutate(tmp = 1) %>% 
    pivot_wider(id_cols = USUBJID, names_from = BC, values_from = tmp, values_fill = 0) %>% 
    arrange(USUBJID) %>% 
    select(-USUBJID) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
}


get_birth_control_since_LV <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, BC, BC_CHANGE) %>% 
    mutate(
      BC = 
        case_when(
          str_detect(BC, ",") & BC_CHANGE ~ "change", 
          str_detect(BC, ",") & !BC_CHANGE ~ "mix",
          TRUE ~ BC
        ),
      tmp_BC = 1
    ) %>% 
    pivot_wider(names_from = BC, values_from = tmp_BC, values_fill = 0, names_prefix = "BC: ") %>%   
    select(-BC_CHANGE) %>% 
    as.data.frame() %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
    
}

