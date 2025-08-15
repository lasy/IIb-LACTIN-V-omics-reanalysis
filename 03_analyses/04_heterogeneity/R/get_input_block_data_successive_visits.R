
get_input_block_data_successive_visits <- function(block, mae, barcodes){
  
  block <- match.arg(block, several.ok = FALSE, choices = get_possible_blocks())
  clin <- MultiAssayExperiment::colData(mae) %>% as.data.frame()
  
  Barcodes <- NULL
  
  if (block %in% c("Arm", "Demographics", "Microbiota (16S topics)", "Cytokines", "Vag. environment")) {
    Barcodes <- barcodes$prev_Barcode
  } 
  if (block %in% c("Birth control", "Adherence", "Antibiotic use", "Perturbations", "Sexual behavior", "Last event")) {
    Barcodes <- barcodes$next_Barcode
  }
  
  if (is.null(Barcodes)) stop("\n@Laura: Fix get_input_block_data_successive_visits to specify which visit should be used for this block")
  
  get_block_data(mae = mae, block = block, Barcodes = Barcodes) %>% 
    set_rownames(barcodes$next_Barcode)
  
}