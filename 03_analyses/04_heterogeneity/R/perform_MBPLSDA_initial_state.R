
perform_MBPLSDA_initial_state <- function(mae, target_visit = 4, arm = "LACTIN-V", mb_nf = NULL, nboot = 200) {
  
  # 1. select participants
  participants <- get_included_participants_initial_state(mae = mae, target_visit = target_visit)
  if (arm != "both") {
    participants <- participants %>% filter(ARM == arm)
  }
  
  # 2. prepare data
  if (arm != "both") {
    input_blocks <- c("Demographics", "Microbiota (16S topics)", "Cytokines") # 
  } else {
    input_blocks <- c("Arm", "Demographics", "Microbiota (16S topics)", "Cytokines")
  }
  
  inputs <- 
    purrr::map(
      .x = input_blocks, 
      .f = get_block_data, 
      mae = mae, IDs = participants$USUBJID, visit = 0
    ) %>% 
    set_names(input_blocks)
  
  output <- 
    get_block_data(
      mae, block = "Endpoint categories", 
      IDs = participants$USUBJID, visit = target_visit
    ) 
  
  # 3. prepare for MB-PLS-DA
  ktabX <- ktab.list.df(inputs)
  dudiY <- dudi.pca(output, center = FALSE, scale = FALSE, scannf = FALSE)
  row.names(ktabX) <- row.names(dudiY$tab)
  
  check <- 
    tibble(
      barcodes_input = row.names(ktabX),
      barcodes_output = row.names(dudiY$tab)
    ) %>% 
    left_join(., colData(mae) %>% as.data.frame() %>% select(Barcode, USUBJID) %>% rename(USUBJID_input = USUBJID), by = c("barcodes_input" = "Barcode")) %>% 
    left_join(., colData(mae) %>% as.data.frame() %>% select(Barcode, USUBJID) %>% rename(USUBJID_output = USUBJID), by = c("barcodes_output" = "Barcode"))
  
  if (!all(check$USUBJID_input == check$USUBJID_output)) stop("Rows from inputs and output tables don't match.\n")
  
  # 4. run MB-PLS-DA
  res_mbplsda <- 
    mbplsda(
      dudiY, ktabX, scale = TRUE, option = "uniform", scannf = FALSE, nf = ktabX$cw %>% length()
    )
  
  # 5. estimate confidence intervals
  if (!is.null(mb_nf)) {
    boot <- boot_mbplsda(res_mbplsda, optdim = mb_nf, nrepet = nboot)
  } else {
    boot <- NULL
  }
  
  list(inputs = inputs, output = output, res = res_mbplsda, boot = boot)
}
