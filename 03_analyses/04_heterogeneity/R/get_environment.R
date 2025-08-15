get_environment <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, LOAD, pH) %>% 
    mutate(
      `log10(bacterial load)` = log10(LOAD %>% add(1) %>% replace_na(mean(LOAD, na.rm = TRUE)))
    ) %>% 
    select(Barcode, `log10(bacterial load)`, pH) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
}