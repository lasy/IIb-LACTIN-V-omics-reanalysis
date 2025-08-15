get_possible_blocks <- function(){
  c("Arm", "Demographics", 
    "Birth control", "Birth control (V1 to V4)", "Birth control (V1 to V7)",
    "Sexual behavior", "Sexual behavior (V1 to V4)", "Sexual behavior (V1 to V7)",
    "Vag. environment",
    "Adherence",  "Adherence (V1 to V4)",  "Adherence (V1 to V7)",
    "Antibiotic use", "Antibiotic use (V1 to V4)", "Antibiotic use (V1 to V7)",
    "Perturbations", "Perturbations (V1 to V4)", "Perturbations (V1 to V7)",
    "Last event",
    "Microbiota (16S topics)", 
    "Cytokines",
    "Endpoint", "Endpoint categories", "BV", "combined", "L. crispatus"
  )
}


get_block_data <- function(mae, block = "Arm", IDs = NULL, visit = NULL, Barcodes = NULL) {
  
  block <- match.arg(block, several.ok = FALSE, choices = get_possible_blocks())
  
  if (is.null(IDs) | is.null(visit)) {
    if (is.null(Barcodes)) stop("If `Barcodes` is NULL, then `IDs` and `visit` cannot be NULL")
    if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
    barcodes <- Barcodes
  } else {
    if (!all(IDs %in% mae$USUBJID)) stop("all `IDs` must be present in mae$USUBJID")
    if (!all(visit %in% mae$AVISITN)) stop("`visit` must be one of mae$AVISITN")
    tmp <- 
      MultiAssayExperiment::colData(mae) %>% 
      as.data.frame() %>% 
      select(USUBJID, AVISITN, Barcode) %>% 
      as_tibble() %>% 
      filter(USUBJID %in% IDs, AVISITN == visit) %>% 
      mutate(USUBJID = USUBJID %>% factor(., levels = IDs)) %>% 
      arrange(USUBJID)
    barcodes <- tmp$Barcode
    if (!is.null(Barcodes) && !all(sort(unique(barcodes)) == sort(unique(Barcodes)))) 
      warning("Provided `Barcodes` don't match those retrieved from the `IDs` and `visit` values.")
  }

  
  res <- 
    switch(
      block,
      Arm = get_arm(mae, Barcodes = barcodes),
      Demographics =  get_demographics(mae, Barcodes = barcodes)$participant_data_dummy,
      `Birth control` = get_birth_control_since_LPIPV(mae, Barcodes = barcodes),
      `Birth control (V1 to V4)` = get_birth_control_throughout(mae, Barcodes = barcodes, target_visit = 4),
      `Birth control (V1 to V7)` = get_birth_control_throughout(mae, Barcodes = barcodes, target_visit = 7),
      Adherence = get_adherence_since_LPIPV(mae, Barcodes = barcodes),
      `Adherence (V1 to V4)` = get_adherence_throughout(mae, Barcodes = barcodes, target_visit = 4),
      `Adherence (V1 to V7)` = get_adherence_throughout(mae, Barcodes = barcodes, target_visit = 7),
      `Antibiotic use` = get_antibiotic_use_since_LPIPV(mae, Barcodes = barcodes),
      `Antibiotic use (V1 to V4)` = get_antiobiotic_use_throughout(mae, Barcodes = barcodes, target_visit = 4),
      `Antibiotic use (V1 to V7)` = get_antiobiotic_use_throughout(mae, Barcodes = barcodes, target_visit = 7),
      Perturbations = get_perturbations_since_LPIPV(mae, Barcodes = barcodes),
      `Perturbations (V1 to V4)` = get_perturbations_throughout(mae, Barcodes = barcodes, target_visit = 4),
      `Perturbations (V1 to V7)` = get_perturbations_throughout(mae, Barcodes = barcodes, target_visit = 7),
      `Vag. environment` = get_environment(mae, Barcodes = barcodes),
      `Sexual behavior` = get_sexual_behavior_since_LPIPV(mae, Barcodes = barcodes),
      `Sexual behavior (V1 to V4)` = get_sexual_behavior_throughout(mae, Barcodes = barcodes, target_visit = 4),
      `Sexual behavior (V1 to V7)` = get_sexual_behavior_throughout(mae, Barcodes = barcodes, target_visit = 7),
      `Last event` = get_last_event(mae, Barcodes = barcodes),
      `Microbiota (16S topics)` = get_assay_block(mae, get_assayname_for_block(block), Barcodes = barcodes),
      Cytokines = get_assay_block(mae, get_assayname_for_block(block), Barcodes = barcodes),
      `Endpoint categories` = get_assay_block(mae, get_assayname_for_block(block), Barcodes = barcodes),
      `Endpoint` = get_assay_block(mae, get_assayname_for_block(block), Barcodes = barcodes),
      BV = get_BV(mae, Barcodes = barcodes),
      combined = get_combined(mae, Barcodes = barcodes),
      `L. crispatus` = get_L_crispatus(mae, Barcodes = barcodes)
    )
  
  res <- res[barcodes, ]

  res
}

get_assayname_for_block <- 
  function(
    blockname = c("Cytokines", "Microbiota (16S topics)", "Endpoint categories", "Endpoint")
  ){
    
    blockname <- match.arg(blockname)
    switch(
      blockname,
      `Microbiota (16S topics)` = "c_topics_16S_8",
      Cytokines = "cytokine_transformed",
      `Endpoint categories` = "endpoints_wide",
      Endpoint = "endpoints_wide"
    )
  }

get_arm <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  tibble(Barcode = mae$Barcode, ARM = mae$ARM) %>% 
    distinct() %>% 
    filter(Barcode %in% Barcodes) %>% 
    arrange(Barcode) %>% 
    mutate(tmp = 1) %>% 
    pivot_wider(id_cols = Barcode, names_from = ARM, values_from = tmp, values_fill = 0) %>% 
    column_to_rownames("Barcode")
}

get_BV <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  tibble(Barcode = mae$Barcode, BV = mae$BV) %>% 
    distinct() %>% 
    filter(Barcode %in% Barcodes) %>% 
    arrange(Barcode) %>% 
    mutate(tmp = 1, BV = ifelse(BV == "Yes", "rBV","no rBV") |>  replace_na("no rBV")) |> 
    pivot_wider(id_cols = Barcode, names_from = BV, values_from = tmp, values_fill = 0) %>% 
    column_to_rownames("Barcode")
}


get_combined <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  get_assay_block(mae, get_assayname_for_block("Endpoint categories"), Barcodes = Barcodes) |> 
    rownames_to_column("Barcode") |>
    left_join(
      MultiAssayExperiment::colData(mae) |> as.data.frame() |> select(Barcode, BV), by = join_by(Barcode)
    ) |>
    mutate(
      across(contains("% L"), ~ifelse(BV == "Yes", 0, .x)),
      BV = ifelse(BV == "Yes", 1, 0) |> replace_na(0)
    ) |> 
    arrange(Barcode) |> 
    column_to_rownames("Barcode")
}

get_L_crispatus <- function(mae, Barcodes){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  get_assay_block(mae, get_assayname_for_block("Endpoint categories"), Barcodes = Barcodes) |> 
    mutate(`< 50% L. crispatus` = `≥ 50% Lactobacillus` + `< 50% Lactobacillus`) |> 
    select(contains("L. crispatus"))
}
