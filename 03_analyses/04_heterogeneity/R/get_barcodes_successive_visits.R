

get_barcodes_successive_visits <- function(mae, arm = c("LACTIN-V", "Placebo","both"), visits, input_blocks, output_block){
  
  arm <- match.arg(arm)
  
  # We need to obtain the Barcodes for the samples of participants 
  # that have data both each pair of the successive visits.
  # In addition
  
  # 1. we first filter by visits and visit arm 
  clin <- 
    MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% as_tibble() %>% 
    filter(
      AVISITN %in% visits
    )
  if (arm != "both") clin <- clin %>% filter(ARM == arm)
  
  # 2. we filter for barcodes such that there is a match between pairs of successive visits.
  
  visits_sequence <- c(0:4, 7)
  if (! all(visits %in% visits_sequence)) stop(str_c("visits must be one of ", str_c(visits_sequence, collapse = ", ")))
  n_visits <- length(visits)
  
  prev_visits <- visits[1:(n_visits-1)]
  prev_visit_barcodes <- 
    clin %>% 
    filter(AVISITN %in% prev_visits) %>% 
    select(USUBJID, AVISITN, Barcode)  %>% 
    dplyr::rename(prev_visit = AVISITN, prev_Barcode = Barcode) %>% 
    mutate(next_visit = visits_sequence[match(prev_visit, visits_sequence) + 1])
  
  next_visits <- visits[2:n_visits]
  next_visit_barcodes <- 
    clin %>% 
    filter(AVISITN %in% next_visits) %>% 
    select(USUBJID, AVISITN, Barcode) %>% 
    dplyr::rename(next_visit = AVISITN, next_Barcode = Barcode) %>% 
    mutate(prev_visit = visits_sequence[match(next_visit, visits_sequence) - 1])
  
  barcodes <- 
    inner_join(
      prev_visit_barcodes, 
      next_visit_barcodes, 
      by = join_by(USUBJID, prev_visit, next_visit)
    )
  
  # 3. In addition, we need to ensure that we have cytokine/16S data for the input/output visits
  
  if ("Cytokines" %in% input_blocks) {
    cytokines_barcodes <- mae[[get_assayname_for_block("Cytokines")]] %>% colnames()
    barcodes <- barcodes %>% filter(prev_Barcode %in% cytokines_barcodes)
  }
  
  if ("Microbiota (16S topics)" %in% input_blocks){
    microbiota_barcodes <- mae[[get_assayname_for_block("Microbiota (16S topics)")]] %>% colnames()
    barcodes <- barcodes %>% filter(prev_Barcode %in% microbiota_barcodes)
  }
  
  if ("Microbiota (16S topics)" %in% output_block){
    microbiota_barcodes <- mae[[get_assayname_for_block("Microbiota (16S topics)")]] %>% colnames()
    barcodes <- barcodes %>% filter(next_Barcode %in% microbiota_barcodes)
  }
  
  # 4. done! we return the barcode list, ordered
  barcodes <- barcodes %>% arrange(prev_visit, USUBJID) %>% mutate(sample_id = row_number())
  barcodes
}