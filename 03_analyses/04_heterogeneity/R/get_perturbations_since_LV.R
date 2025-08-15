


get_perturbations_since_LPIPV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  tmp <- 
    MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, DAY, MENSES, LAST_M, N_BLEED_LPIPV, N_SINCE_LPIPV_DOUCH, LAST_DOUCH) 
  
  n_distinct_values <- apply(tmp, 2, function(x) length(unique(x)))
  tmp <- tmp[,n_distinct_values > 1]
  if ("MENSES" %in% colnames(tmp)) 
    tmp <- tmp %>% mutate(`Is menstruating` = MENSES %>% multiply_by(1))
  if ("LAST_M" %in% colnames(tmp)) 
    tmp <- tmp %>% mutate(`Days since last menses` = LAST_M)
  
  tmp <- 
    tmp %>% 
    mutate(
      `N days bleeding` = N_BLEED_LPIPV,
      `N douching` = N_SINCE_LPIPV_DOUCH,
      `Douching in last 7 days` = ((LAST_DOUCH |> replace_na(100)) < 7) * 1
    ) %>% 
    select(Barcode,  matches("[a-z]$", ignore.case = FALSE)) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  tmp
  
}



get_perturbations_throughout <-function(mae, Barcodes, target_visit){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  P_colnames <- c("BLEEDING", "DOUCHING")
  if (target_visit == 4) P_colnames <- str_c(P_colnames, "_W1_12")
  
  res <- 
    MultiAssayExperiment::colData(mae) |> 
    as.data.frame() |> 
    filter(Barcode %in% Barcodes) |> 
    arrange(Barcode) |> 
    select(all_of(P_colnames))
  
  colnames(res) <- c("Bleeding", "Douching")
  res 
}



get_perturbations_since_LV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  tmp <- 
    MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, MENSES, LAST_M, SINCE_LV_DOUCH, LAST_DOUCH) 
  
 n_distinct_values <- apply(tmp, 2, function(x) length(unique(x)))
 tmp <- tmp[,n_distinct_values > 1]
 if ("MENSES" %in% colnames(tmp)) 
   tmp <- tmp %>% mutate(`Is menstruating` = MENSES %>% multiply_by(1))
 if ("LAST_M" %in% colnames(tmp)) 
   tmp <- tmp %>% mutate(`Days since last menses` = LAST_M %>% replace_na(max(LAST_M, na.rm = TRUE) + 1))
 
  tmp <- 
    tmp %>% 
    mutate(
      `Douched since last visit` = SINCE_LV_DOUCH %>% replace_na(FALSE) %>% multiply_by(1),
      `Days since last douching` = LAST_DOUCH %>% replace_na(2 * max(LAST_DOUCH, na.rm = TRUE))
    ) %>% 
    select(Barcode,  matches("[a-z]$", ignore.case = FALSE)) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  tmp
  
}

