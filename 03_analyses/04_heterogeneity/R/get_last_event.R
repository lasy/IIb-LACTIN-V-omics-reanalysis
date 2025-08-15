get_last_event <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  tmp <- 
    MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, ends_with("IS_LAST"))
  
  colnames(tmp) <- 
    colnames(tmp) |> str_remove("_IS_LAST") |>  str_to_sentence() |> 
    str_replace("M", "Menstruation") |> str_replace("rot_sex", "rot. sex") |> 
    str_replace("Ster", "Steroid drugs") |> 
    str_replace("Afun", "Anti-fungal drugs") |>
    str_replace("_abio", " abx") |> 
    str_replace("O", "Oral") |> 
    str_replace("V", "Vaginal") |>
    str_replace("Douch", "Douching")
    
  tmp <- 
    tmp |> 
    as.data.frame() |> 
    set_rownames(NULL) |> 
    arrange(Barcode) |> 
    column_to_rownames("Barcode")
  
  tmp
}