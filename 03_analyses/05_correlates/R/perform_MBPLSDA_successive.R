perform_MBPLSDA_successive <- function(model_par, mbplsda_par, IO_par = IO_par, verbose = TRUE) {
  
  # 0. Input parameters
  # 0.1 model parameters
  seed <- model_par$seed
  if (is.null(seed)) seed <- 1
  set.seed(seed)
  
  mae <- model_par$mae
  output_var <- match.arg(model_par$output_var, c("microbiota categories", "L. crispatus", "rBV", "combined"))
  target_visit <- model_par$target_visit
  if (!(target_visit %in% c(2, 3, 4, 7))) stop("`model_par$target_visit` must be 2, 3, 4 or 7.\n")
  if (target_visit %in% c(3, 4)) target_visits <- c(3, 4) else target_visits <- target_visit
  arm <- match.arg(model_par$arm, c("Lactin-V", "placebo", "both"))
  input_blocks <- model_par$input_blocks # should be NULL for the code to pick automatically
  model <- match.arg(model_par$model, c("full", "extended", "reduced", "minimal", "self-pred"))
  scaling <- model_par$scaling # NULL??
  
  if (is.null(model_par$model_name)) {
    model_name <- 
      str_c(
        "MBPLSDA in ", arm, " arm", ifelse(arm == "both","s",""),
        " for ",output_var, 
        " at ", get_visit_labels(target_visits) |> str_c(collapse = " and "),
        ifelse(model != "full", str_c(" (", model, " model)"), ""), 
        " with seed ", seed
        )
  } else {model_name <- model_par$model_name}
  if (verbose) cat("\t", model_name, "\n")
  microbiota_assay <- model_par$microbiota_assay
  
  # 0.2 mbplsda parameters
  max_ncomp <- mbplsda_par$max_ncomp
  if (is.null(max_ncomp)) {warning("`mbplsda_par$max_ncomp` is NULL, using default value of 10."); max_ncomp <- 10}
  opt_ncomp <- mbplsda_par$opt_ncomp
  opt_ncomp_max <- mbplsda_par$opt_ncomp_max
  if (is.null(opt_ncomp_max)) {
    warning(str_c("`mbplsda_par$opt_ncomp_max` is NULL, using default value of ", max_ncomp, " (the max number of components)")); 
    opt_ncomp_max <- max_ncomp
    }
  cv_nrepet <- mbplsda_par$cv_nrepet
  cv_prop <- mbplsda_par$cv_prop
  npermut <- mbplsda_par$npermut
  nboot <- mbplsda_par$nboot
  cpus <- mbplsda_par$cpus
  
  # 0.3 IO parameters
  save_results <- IO_par$save
  output_dir <- IO_par$output_dir
  if (save_results & !dir.exists(output_dir)) dir.create(output_dir)
  force_execution <- IO_par$force_execution
  
  
  filename <- str_c(output_dir, model_name, ".RDS")
  if (file.exists(filename) & !force_execution) {
    res <- readRDS(filename)
    if(verbose) cat("File already exists. Use `IO_par$force_execution <- TRUE` to overwrite.\n")
  } 
  if (!file.exists(filename) | force_execution) {
    if (verbose) cat("File does not exist or `IO_par$force_execution = TRUE`. Running MBPLSDA...\n")
    res <- 
      perform_MBPLSDA_successive_internal(
        mae = mae, 
        output_var = output_var, 
        target_visits = target_visits, 
        arm = arm, 
        input_blocks = input_blocks,
        microbiota_assay = microbiota_assay,
        model = model,
        scaling = scaling,
        max_ncomp = max_ncomp, 
        opt_ncomp = opt_ncomp, 
        opt_ncomp_max = opt_ncomp_max,
        cv_nrepet = cv_nrepet, 
        cv_prop = cv_prop,
        npermut = npermut, 
        nboot = nboot, 
        cpus = cpus, 
        model_name = model_name, 
        verbose = verbose,
      )
    res$filename <- filename
    res$model_par <- model_par
    res$mbplsda_par <- mbplsda_par
    if (save_results & verbose) cat("Saving results in ",filename,"...\n")
    if (save_results) saveRDS(res, file = filename)
  }
  res  
}


perform_MBPLSDA_successive_internal <- function(
    mae, output_var, target_visits, arm, input_blocks, microbiota_assay, model, model_name, scaling,
    max_ncomp, opt_ncomp, opt_ncomp_max, cv_nrepet, cv_prop, npermut, nboot, cpus, 
    verbose, shuffle_var_blocks){
  
  # 1. prepare data
  if (verbose) cat("Prepare block data...\n")
  
  if (is.null(input_blocks)) input_blocks <- get_input_blocks_successive()
  if (arm == "both") input_blocks <- c("Arm", input_blocks)
  
  if (arm == "Lactin-V") PID <- mae$USUBJID[mae$ARM == "LACTIN-V"] |> unique() 
  if (arm == "placebo") PID <- mae$USUBJID[mae$ARM == "Placebo"] |> unique()
  if (arm == "both") PID <- mae$USUBJID |> unique()
  PID <- PID[!is.na(PID)]
  
  tmp <- 
    map(
      .x = input_blocks, 
      .f = get_block_data_successive, 
      mae = mae, PID = PID, 
      target_visits = target_visits, 
      microbiota_assay = microbiota_assay
      )
  inputs_descr <- map(tmp, ~.x$descr) |> bind_rows()
  inputs_raw <- 
    map(tmp, ~.x$data) |> 
    set_names(input_blocks) |> 
   complete_cases() 
  
  inputs <- 
    inputs_raw |> 
    scale_inputs(scaling = scaling) |> 
    add_tiny_noise() 
  
  output <- get_block_data(mae, block = output_var, PID = PID, target_visits = target_visits)
  cat(class(output))
  
  common_samples <- intersect(rownames(output), rownames(inputs[[1]]))
  inputs_raw <- map(inputs_raw, ~.[common_samples,])
  inputs <- map(inputs, ~.[common_samples,])
  output <- output[common_samples,]
  
  output_fct <- colnames(output)[apply(output, 1, which.max)] |> factor(levels = colnames(output))
  
  
  if (model == "extended") inputs <- inputs[-1]
  if (model == "reduced") inputs <- inputs[5:length(inputs)]
  if (model == "minimal") inputs <- inputs[5:8]
  if (model == "self-pred") inputs <- inputs[5]
  
  
  cat("\t nb of explanatory blocks",length(inputs), "\n")
  
  
  if ((7 %in% target_visits) & ("Adherence" %in% names(inputs))) inputs <- inputs[-which(names(inputs) == "Adherence")]
  # if (target_visits == 7) {
  #   cat("here\n")
  #   j <- which(names(inputs) == "Adherence")
  #   cat(j, "\n")
  #   cat(length(inputs), "\n")
  #   inputs <- inputs[-j]
  #   cat(length(inputs), "\n")
  # }
  

  # 2. prepare for MB-PLS-DA
  ktabX <<- ktab.list.df(inputs) 
  dudiY <<- dudi.pca(output, center = FALSE, scale = FALSE, scannf = FALSE) # <<-
  
  # 3. run baseline MB-PLS-DA
  if (verbose) cat("Fit basal MB-PLS-DA model...\n")
  res_mbplsda <- mbplsda(dudiY, ktabX, scale = FALSE, option = "none", scannf = FALSE, nf = max_ncomp)
  nbloY <- ncol(output)
  
  # 4.1 Use cross-validation to identify the optimal number of components
  if (is.null(opt_ncomp)) {
    if (verbose) cat("Cross-validation to identify optimal number of components...\n")
    if (verbose) cat(str_c("\t", cv_nrepet," fold with ",round(100*cv_prop),"% (c) - ",100 - round(100*cv_prop),"% (v) random splits\n"))
    # res_testdim <- testdim_mbplsda(object = res_mbplsda, nrepet = cv_nrepet, bloY = nbloY, cpus = cpus, algo = "max")
    # cv_error_rates <- get_cv_error_rates(res_testdim)
    # opt_ncomp <- select_opt_ncomp(cv_error_rates) # cv_error_rates |> filter(set == "validation") |> filter(mean == min(mean, na.rm = TRUE)) |> pull(n) |> head(1)
    
    # set.seed(1)
    cv <- cv_mbplsda(object = res_mbplsda, nrepet = cv_nrepet, prop = cv_prop)
    if (verbose) cat("\tCriteria: maximization of the average F1 score\n")
    cv_metric <- cv$summary |> filter(metric == "average F1 score") |> mutate(n_samples = n, n = dim, mean = -mean)
    opt_ncomp <- min(opt_ncomp_max, select_opt_ncomp(cv_metric))
    if (verbose) cat("\tOptimal number of components (CV):", opt_ncomp,"\n")
  } else {
    cv <- NULL
    cv_error_rates <- NULL
    if (verbose) cat("\tOptimal number of components (user selected):", opt_ncomp,"\n")
  }
  res_mbplsda$nf <- opt_ncomp
  
  # 4.2 Use permutation tests to assess the significance of the model
  # if (npermut > 0) {
  #   if (verbose) cat("Permutation test for significance of the model...\n")
  #   res_permut <- permut_mbplsda(object = res_mbplsda, optdim = opt_ncomp, bloY = nbloY,
  #                                nrepet = npermut, npermut = 20, nbObsPermut = nrow(output), cpus = cpus,
  #                                algo = "max")
  #   permut_pval <- compute_permut_pval(res_permut) 
  #   if (verbose) cat("\tPermutation test p-value:", permut_pval |> format.pval(eps = 1e-5),"\n")
  # } else {
     # res_permut <- NULL
     # permut_pval <- NA
  #   if (verbose) cat("\tNo permutation test was performed. Set `npermut` to a positive integer to perform a permutation test.\n")
  # }
  
  
  # 5. estimate confidence intervals
  if (nboot > 0) {
    if (verbose) cat("Bootstrap to estimate confidence intervals...\n")
    boot <- my_boot_mbplsda(res_mbplsda, optdim = opt_ncomp, nrepet = nboot, cpus = cpus)
  } else {
    if (verbose) cat("\tBootstrap was not performed. Set `nboot` to a positive integer to estimate boostrapped confidence intervals.\n")
    boot <- NULL
  }
  
  # 6. predicted output
  if (verbose) cat("Predictions and formatting results...\n")
  predictions <- 
    pred_mbplsda(object = res_mbplsda, optdim = opt_ncomp, bloY = nbloY, algo = "max") |> 
    format_predictions()
  
  # 7. return results
  list(
    inputs_descr = inputs_descr,
    inputs_raw = inputs_raw, inputs = inputs, 
    output = output, output_fct = output_fct,
    res = res_mbplsda, 
    cv = cv, opt_ncomp = opt_ncomp,
    # res_permut = res_permut, permut_pval = permut_pval,
    boot = boot, 
    model_name = model_name,
    predictions = predictions
  )
}
