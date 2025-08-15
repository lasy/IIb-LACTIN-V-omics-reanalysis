get_antibiotic_use_since_LPIPV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, AVISITN, LAST_V_ABIO, N_SINCE_LPIPV_V_ABIO, LAST_O_ABIO, N_SINCE_LPIPV_O_ABIO) %>% 
    mutate(
      `Any vag. abx` = (replace_na(N_SINCE_LPIPV_V_ABIO,0) > 0)*1,
      `Any vag. abx in last 7 days` = (replace_na(LAST_V_ABIO, 100) < 7)*1,
      `Any oral abx` = (replace_na(N_SINCE_LPIPV_O_ABIO,0) > 0)*1,
      `Any oral abx in last 7 days` = (replace_na(LAST_O_ABIO, 100) < 7)*1
    ) %>% 
    select(Barcode,  `Any vag. abx`, `Any vag. abx in last 7 days`,  `Any oral abx`, `Any oral abx in last 7 days`) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}


get_antiobiotic_use_throughout <-function(mae, Barcodes, target_visit){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  A_colnames <- c("ADD_V_ABIO_W1_12","ADD_O_ABIO_W1_12")
  if (target_visit == 7) A_colnames <- c(A_colnames, "ADD_V_ABIO_W13_24","ADD_O_ABIO_W13_24")
  
  res <- 
    MultiAssayExperiment::colData(mae) |> 
    as.data.frame() |> 
    filter(Barcode %in% Barcodes) |> 
    arrange(Barcode) |> 
    select(all_of(A_colnames))
  
  colnames(res) <- 
    colnames(res) |> 
    str_replace_all("ADD_", "Add. ") |>
    str_replace_all("ABIO_", "abx ") |>
    str_replace_all("V_", "vag. ") |>
    str_replace_all("O_", "oral ") |>
    str_replace_all("W1_12", "weeks 1-12") |>
    str_replace_all("W13_24", "weeks 13-24")
  
  res
  
}






# Deprecated


get_antibiotic_use_since_LV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, AVISITN, SINCE_LV_V_ABIO, SINCE_LV_O_ABIO) %>% 
    mutate(
      `Vaginal antibio.` = SINCE_LV_V_ABIO %>% replace_na(FALSE) %>% multiply_by(1),
      `Oral antibio.` = SINCE_LV_O_ABIO %>% replace_na(FALSE) %>% multiply_by(1)
    ) %>% 
    select(Barcode,  `Vaginal antibio.`, `Oral antibio.`) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}
