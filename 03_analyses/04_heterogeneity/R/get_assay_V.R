

get_assay_block <- function(mae, assayname, Barcodes){
  get_assay_wide_format(mae, assayname, add_colData = FALSE) %>% 
    filter(Barcode %in% Barcodes) %>% 
    unnest(cols = c(assay)) %>% 
    arrange(Barcode) %>% 
    column_to_rownames(var = "Barcode") 
}


get_topics_V0 <- function(mae, assayname, IDs){
  get_assay_wide_format(mae, assayname) %>% 
    filter(
      USUBJID %in% IDs,
      AVISITN == 0
    ) %>% 
    arrange(USUBJID) %>% 
    select(assay) %>% 
    unnest(cols = c(assay)) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
}
