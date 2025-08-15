

perform_MBPLS_successive_visits <- function(mae, visits = 1:4, arm = c("LACTIN-V", "Placebo", "both"), mb_nf = NULL, nboot = 200, model_name = NULL, verbose = FALSE) {
  
  arm <- match.arg(arm)
  if (is.null(model_name)) model_name <- str_c("MBPLS_successive_V_", arm, "_arm_and_visits_", str_c(visits, collapse = "_"))
  
  
  # 1. define input and output blocks
  if (verbose) cat("\nDefining input and output blocks...")
  
  input_blocks <- 
    c(
      "Demographics", "Birth control", 
      "Adherence", "Antibiotic use", 
      "Perturbations", "Sexual behavior", "Last event",
      "Vag. environment", 
      "Microbiota (16S topics)", "Cytokines"
    )
  if (arm == "both") input_blocks <-  c("Arm", input_blocks)
  
  output_block <- "Microbiota (16S topics)"
  
  
  # 2. select barcodes
  if (verbose) cat("\nSelecting barcodes...")
  
  barcodes <- 
    get_barcodes_successive_visits(
      mae = mae, arm = arm,
      visits = visits, 
      input_blocks = input_blocks, output_block = output_block
      )

  
  # 3. prepare data
  if (verbose) cat("\nPreparing data...")
  
  
  inputs <- 
    purrr::map(
      .x = input_blocks, 
      .f = get_input_block_data_successive_visits, 
      mae = mae, barcodes = barcodes
    ) %>% 
    set_names(input_blocks) %>% 
    add_tiny_noise()
  
  
  output <- 
    get_block_data(
      mae, block = "Microbiota (16S topics)", Barcodes = barcodes$next_Barcode
    ) %>% 
    set_rownames(barcodes$next_Barcode)
  
  # 4. prepare for MB-PLS
  if (verbose) cat("\nPreparing for MB-PLS...")
  
  ktabX <- ktab.list.df(inputs) 
  dudiY <- dudi.pca(output, center = TRUE, scale = TRUE, scannf = FALSE)

  # 5. run MB-PLS
  if (verbose) cat("\nRun MB-PLS...")
  
  res_mbpls <- 
    mbpls(
      dudiY = dudiY, ktabX = ktabX, 
      scale = TRUE, option = "uniform", 
      scannf = is.null(mb_nf), 
      nf = ifelse(is.null(mb_nf), 2, mb_nf)
    )
  
  # 6. estimate confidence intervals
  if (verbose) cat("\nEstimate confidence intervals...")
  if (!is.null(mb_nf)) {
    boot <- randboot(res_mbpls, optdim = mb_nf, nrepet = nboot)
  } else {
    boot <- NULL
  }
  
  if (verbose) cat("\nDone!")
  
  list(model_name = model_name, inputs = inputs, output = output, res = res_mbpls, boot = boot)
}


add_tiny_noise <- function(inputs){
  purrr::map(
      .x = inputs,
      .f = function(X){
        sds <- apply(X, 2, sd) + 1/1000
        tiny_noise <- sapply(seq_along(sds), function(i) rnorm(nrow(X), mean = 0, sd = sds[i] / 1000)) 
        X + tiny_noise
      }
    )
}
