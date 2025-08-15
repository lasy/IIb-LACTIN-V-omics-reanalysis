
perform_MBPLSDA_success <- 
  function(mae, 
           output_var = c("L. crispatus", "BV", "combined", "Endpoint"), 
           target_visit = 4, arm = c("LACTIN-V", "Placebo","both"), 
           mb_nf = NULL, nboot = 200, model_name = NULL) {
    
    
    output_var <- match.arg(output_var)
    arm <- match.arg(arm)
    if (is.null(model_name)) model_name <- str_c("MBPLSDA in ", arm, " arm",ifelse(arm == "both","s","")," for ",output_var, " at ", get_visit_labels(target_visit))
    
    
    # 1. select participants
    participants <- 
      get_included_participants_success(mae = mae, target_visit = target_visit)
    if (arm != "both") {
      participants <- participants %>% filter(ARM == arm)
    }
    
    # 2. prepare data
    input_blocks <- 
      c(
        "Demographics", 
        "Birth control", 

        "Microbiota (16S topics) pre-MTZ", 
        "Cytokines pre-MTZ",
        "Microbiota (16S topics) post-MTZ", 
        "Cytokines post-MTZ",
        "Vag. environment post-MTZ",
        
        "Adherence", "Antibiotic use", 
        "Perturbations", "Sexual behavior"
        )
    if (arm == "both") input_blocks <- c("Arm", input_blocks)
    
    inputs <- 
      purrr::map(
        .x = input_blocks, 
        .f = get_block_data_success, 
        mae = mae, IDs = participants$USUBJID, target_visit = target_visit
      ) %>% 
      set_names(input_blocks)
    
    output <- 
      get_block_data(
        mae, block = output_var, 
        IDs = participants$USUBJID, visit = target_visit
      ) 
    rownames(output) <- mae$USUBJID[match(rownames(output),mae$Barcode)]
    
    if (!all (rownames(output) == rownames(inputs[[1]]))) stop("inputs and output rows don't match!")
    
    # 3. prepare for MB-PLS-DA
    ktabX <- ktab.list.df(inputs)
    dudiY <- dudi.pca(output, center = FALSE, scale = FALSE, scannf = FALSE)
    # row.names(ktabX) <- row.names(dudiY$tab)
    
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
    
    # 6. predicted output
    outcomes <- colnames(output) |> factor(levels = colnames(output))
    true_outcomes <- outcomes[apply(output, 1, which.max)]
    
    pred_outcomes_all <- mtb_predict(res = res_mbplsda, original_X = inputs, original_Y = output) 
    pred_outcomes_all <- 
      pred_outcomes_all |> 
      as.data.frame() |> 
      rownames_to_column("USUBJID") |> 
      mutate(true_outcome = true_outcomes) |> 
      pivot_longer(cols = -c(true_outcome, USUBJID), names_to = "predicted_outcome", values_to = "predicted_value") |> 
      mutate(predicted_outcome = predicted_outcome |> factor(levels = outcomes))
    
    res_mbplsda$nf <- MBViz::get_mtb_eig(res_mbplsda) |> filter(cumvar > 0.95) |> extract2(1,"Ax")
    pred_outcomes_95 <- mtb_predict(res = res_mbplsda, original_X = inputs, original_Y = output) 
    pred_outcomes_95 <- 
      pred_outcomes_95 |> 
      as.data.frame() |> 
      rownames_to_column("USUBJID") |> 
      mutate(true_outcome = true_outcomes) |> 
      pivot_longer(cols = -c(true_outcome, USUBJID), names_to = "predicted_outcome", values_to = "predicted_value") |> 
      mutate(predicted_outcome = predicted_outcome |> factor(levels = outcomes))
    
    
    res_mbplsda$nf <- mb_nf
    pred_outcomes_nf <- mtb_predict(res = res_mbplsda, original_X = inputs, original_Y = output) 
    pred_outcomes_nf <- 
      pred_outcomes_nf |> 
      as.data.frame() |> 
      rownames_to_column("USUBJID") |> 
      mutate(true_outcome = true_outcomes) |> 
      pivot_longer(cols = -c(true_outcome, USUBJID), names_to = "predicted_outcome", values_to = "predicted_value") |> 
      mutate(predicted_outcome = predicted_outcome |> factor(levels = outcomes))
    
    
    # 7. return results
    
    list(
      inputs = inputs, output = output, 
      res = res_mbplsda, boot = boot, 
      model_name = model_name,
      pred_outcomes_all_nf = pred_outcomes_all,
      pred_outcomes_95 = pred_outcomes_95,
      pred_outcomes = pred_outcomes_nf
      )
  }


get_block_data_success <- function(block, mae, IDs, target_visit = target_visit){
  if (block %in% c("Arm", "Demographics")) res <- get_block_data(mae = mae, block = block, IDs = IDs, visit = 0)
  if (str_detect(block, "pre-MTZ")){
    res <- get_block_data(mae = mae, block = str_remove(block, " pre-MTZ"), IDs = IDs, visit = 0)
    colnames(res) <- colnames(res) |> str_c(" (pre-MTZ)")
    }
  if (str_detect(block, "post-MTZ")){
    res <- get_block_data(mae = mae, block = str_remove(block, " post-MTZ"), IDs = IDs, visit = 1)
    colnames(res) <- colnames(res) |> str_c(" (post-MTZ)")
  }
  if (block %in% c("Birth control", "Adherence", "Antibiotic use","Perturbations", "Sexual behavior")) {
    res <- get_block_data(mae = mae, block = str_c(block," (V1 to V",target_visit,")"), IDs = IDs, visit = 0)
  }
  # use participant ID as rownames and make sure they are sorted by USUBJID
  j <- match(rownames(res), mae$Barcode)
  rownames(res) <- mae$USUBJID[j]
  o <- order(rownames(res))
  res <- res[o,]
  res
}
