get_input_blocks_successive <- function(){
  c(
    "Demographics", 
    
    "Vag. env. pre-MTZ", "Microbiota pre-MTZ",  "Cytokines (r) pre-MTZ", 
    "Coloniz. cat. prev. v." , "Vag. env. prev. v.", "Microbiota (r) prev. v.",  "Cytokines (r) prev. v.", 
    
    "Birth control", "Sexual behavior", "Perturbations", "Antibiotics", "Adherence"
    )
}


get_block_data_successive <- function(block, mae, PID, target_visits, microbiota_assay){
  # cat(block,"\n")
  
  if (block == "Demographics") res <- get_demographics(mae, PID, target_visits)
  
  if (block == "Microbiota pre-MTZ") res <- get_baseline_microbiota(mae, PID, target_visits, microbiota_assay = microbiota_assay)
  if (block == "Vag. env. pre-MTZ") res <- get_baseline_environment(mae, PID, target_visits)
  if (block == "Cytokines (r) pre-MTZ") res <- get_baseline_cytokines(mae, PID, target_visits)
  
  if (block == "Coloniz. cat. prev. v.") res <- get_prev_visit_category(mae, PID, target_visits)
  if (block == "Vag. env. prev. v.") res <- get_prev_visit_environment(mae, PID, target_visits)
  if (block == "Microbiota (r) prev. v.") res <- get_prev_visit_microbiota(mae, PID, target_visits, microbiota_assay = microbiota_assay)
  if (block == "Cytokines (r) prev. v.") res <- get_prev_visit_cytokines(mae, PID, target_visits)
  
  if (block == "Adherence") res <- get_adherence(mae, PID, target_visits)
  if (block == "Birth control") res <- get_birth_control(mae, PID, target_visits)
  if (block == "Perturbations") res <- get_perturbations(mae, PID, target_visits)
  if (block == "Antibiotics") res <- get_antibiotics(mae, PID, target_visits)
  if (block == "Sexual behavior") res <- get_sex(mae, PID, target_visits)
  
  if (str_detect(block, "pre-MTZ")) colnames(res$data) <- colnames(res$data) |> str_c(" (pre-MTZ)")
    
    
  res
}



get_sex <- function(mae, PID, target_visits) {
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits, !is.na(BC)) |> 
    mutate(key = paste(USUBJID, AVISITN, sep = "_")) |> arrange(key) |> 
    select(key, N_SINCE_LPIPV_UNPROT_SEX, N_SINCE_LPIPV_PROT_SEX, N_NEW_PARTNERS) |> 
    distinct() |> 
    mutate(
      ANY_UNPROT = (N_SINCE_LPIPV_UNPROT_SEX > 0) * 1,
      ANY_PROT = (N_SINCE_LPIPV_PROT_SEX > 0) * 1
    ) |> 
    select(-N_SINCE_LPIPV_UNPROT_SEX, -N_SINCE_LPIPV_PROT_SEX) |> 
    dplyr::rename(
      `Any condomless sex` = ANY_UNPROT, 
      `Any condom sex` = ANY_PROT,
      `N new partners` = N_NEW_PARTNERS
    ) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    bind_rows(
      tibble(variable = "N new partners", descr = 'Number of new sexual partners since the last visit.'),
      tibble(variable = "Any condomless sex", descr = 'A flag indicating whether the participant reported any sexual intercourse without condoms since the last visit.'),
      tibble(variable = "Any condom sex", descr = 'A flag indicating whether the participant reported any sexual intercourse with condoms since the last visit.')
    ) |> 
    mutate(block = "Sexual behavior") |> select(block, everything())
  
  list(data = clin, descr = descr) 
}


get_antibiotics <- function(mae, PID, target_visits) {
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits, !is.na(BC)) |> 
    mutate(key = paste(USUBJID, AVISITN, sep = "_")) |> arrange(key) |> 
    select(key, N_SINCE_LPIPV_V_ABIO, N_SINCE_LPIPV_O_ABIO) |> 
    distinct() |> 
    dplyr::rename(`N vag. abx.` = N_SINCE_LPIPV_V_ABIO, `N oral abx.` = N_SINCE_LPIPV_O_ABIO) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    bind_rows(
      tibble(variable = "N vag. abx", descr = 'Number of vaginal antibiotics doses (= days it was applied) since the previous visit.'),
      tibble(variable = "N oral abx", descr = "Number of oral antibiotics doses (= days it was ingested) since the previous visit.")
    ) |> 
    mutate(block = "Antibiotics") |> select(block, everything())
  
  list(data = clin, descr = descr) 
}


get_perturbations <- function(mae, PID, target_visits) {
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits, !is.na(BC)) |> 
    mutate(key = paste(USUBJID, AVISITN, sep = "_")) |> arrange(key) |> 
    select(key, N_SINCE_LPIPV_DOUCH, N_BLEED_LPIPV) |> 
    distinct() |> 
    dplyr::rename(`N douching` = N_SINCE_LPIPV_DOUCH, `N bleeding` = N_BLEED_LPIPV) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    bind_rows(
      tibble(variable = "N douching", descr = 'Number of douching participant declared since the last visit.'),
      tibble(variable = "N bleeding", descr = "Number of days participant reported vaginal bleeding since the last visit.")
      ) |> 
    mutate(block = "Perturbations") |> select(block, everything())
  
  list(data = clin, descr = descr) 
}



get_birth_control <- function(mae, PID, target_visits) {
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits, !is.na(BC)) |> 
    mutate(key = paste(USUBJID, AVISITN, sep = "_")) |> arrange(key) |> 
    select(key, BC) |> 
    distinct() |> 
    mutate(BC = BC |> fct_infreq()) |> 
    arrange(BC) |> mutate(tmp = 1) |> pivot_wider(names_from = BC, values_from = tmp, values_fill = 0) |> 
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    bind_rows(
      tibble(variable = "Non-hormonal", descr = 'Non-hormonal birth control including abstinence, use of condoms, or fertility awareness methods.'),
      tibble(variable = "IUD (Hormonal)", descr = "Hormonal intra-uterine device."),
      tibble(variable = "IUD (Non-hormonal)", descr = "Non-hormonal intra-uterine device (copper IUD)."),
      tibble(variable = "Combined", descr = "Participant used a combination of hormonal contraceptives or switched contraceptive between the last visit and this one. "),
      tibble(variable = "P only", descr = "Progestin-only hormonal contraceptive, including the pill, patches, and implants."),
      tibble(variable = "unknown", descr = "Participant's birth control is unknown (missing data). Each participant belongs to only on of each birth control categories.")
    ) |> 
    mutate(block = "Birth control") |> select(block, everything())
  
  list(data = clin, descr = descr) 
}


get_adherence <- function(mae, PID, target_visits) {
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits) |> 
    mutate(key = paste(USUBJID, AVISITN, sep = "_")) |>
    arrange(key) |> 
    select(key, N_MISSED_DOSES, LAST_DOSE, NDAY_LPIPV) |> 
    distinct() |> 
    mutate(
      LAST_DOSE = ifelse(is.na(LAST_DOSE), NDAY_LPIPV, LAST_DOSE)
    ) |> 
    select(-NDAY_LPIPV) |> 
    dplyr::rename(
      `N missed doses` = N_MISSED_DOSES,
      `Days since last dose` = LAST_DOSE
    ) |> 
    as.data.frame() |> 
    column_to_rownames("key")

  descr <- 
    bind_rows(
      tibble(variable = "N missed doses", descr = 'Difference between the number of "per protocol" doses that should have been taken between the previous visit and the current visit and the actual number of doses that were taken.'),
      tibble(variable = "Days since last dose", descr = "Number of days since last dose was taken.")
    ) |> 
    mutate(block = "Adherence") |> select(block, everything())
  
  list(data = clin, descr = descr) 
}


get_default_shannon_assay <- function() "tax_16S_p"

get_baseline_environment <- function(mae, PID, target_visits){
  
  shannon <- 
    get_assay_wide_format(mae, get_default_shannon_assay()) |> 
    filter(USUBJID %in% PID, AVISITN == 0) |> 
    select(USUBJID, assay) |> 
    mutate(`α diversity` = assay |> vegan::diversity("shannon")) |> 
    select(USUBJID, `α diversity`)
  
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN == 0, !is.na(pH)) |> 
    select(USUBJID, pH) |> 
    distinct()
  
  tmp <- 
    expand_grid(USUBJID = unique(shannon$USUBJID), AVISITN = target_visits) |>
    mutate(key = str_c(USUBJID, "_", AVISITN)) |> select(-AVISITN) |> 
    inner_join(shannon, by = join_by(USUBJID)) |> 
    inner_join(clin, by = join_by(USUBJID)) 
  
  tmp <- tmp  |> 
    select(-USUBJID) |>
    as.data.frame() |> 
    column_to_rownames("key")

  descr <- 
    bind_rows(
      tibble(variable = "α diversity", descr = "Shannon diversity index of baseline (pre-MTZ) microbiota, computed on taxa proportions quantified from 16S rRNA gene sequencing data."),
      tibble(variable = "pH", descr = "Vaginal pH at the pre-MTZ visit, measured at study sites.")
    ) |> 
    mutate(block = "Vag. env. pre-MTZ") |> select(block, everything())

  list(data = tmp, descr = descr)  
}


get_prev_visit_environment <- function(mae, PID, target_visits){
  
  PIPV <- mae$AVISITN[mae$PIPV] |> unique(); PIPV <- PIPV[!is.na(PIPV)] |> sort()
  visits <- tibble(target_visit = PIPV) |> mutate(AVISITN = lag(target_visit)) |> 
    filter(target_visit %in% target_visits) 
  
  categories <- 
    get_assay_long_format(mae, "mb_categories") |> 
    filter(USUBJID %in% PID) |> 
    select(USUBJID, AVISITN, value) |> 
    dplyr::rename("category" = value) 
  
  shannon <- 
    get_assay_wide_format(mae, get_default_shannon_assay()) |> 
    filter(USUBJID %in% PID, AVISITN %in% visits$AVISITN) |> 
    select(USUBJID, AVISITN, assay) |> 
    mutate(`α diversity` = assay |> vegan::diversity("shannon")) |> 
    select(USUBJID, AVISITN, `α diversity`) |> 
    arrange(USUBJID, AVISITN) |> 
    left_join(categories, by = join_by(USUBJID, AVISITN)) |> 
    mutate(
      pred_shannon = lm(`α diversity` ~ category) |> predict(),
      `α diversity (r)` = `α diversity` - pred_shannon
    ) |> 
    select(USUBJID, AVISITN, `α diversity (r)`)
  
  clin <- 
    MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble() |> 
    filter(USUBJID %in% PID, AVISITN %in% visits$AVISITN, !is.na(pH), !is.na(LOAD)) |> 
    select(USUBJID, AVISITN, pH, LOAD) |> 
    distinct() |> 
    mutate(`log10(bact. load)` = log10(LOAD + 1)) |>
    select(-LOAD) |> 
    inner_join(categories, by = join_by(USUBJID, AVISITN)) |> 
    mutate(
      pred_pH = lm(pH ~ category) |> predict(),
      `pH (r)` = pH - pred_pH
    ) |> 
    select(USUBJID, AVISITN, `pH (r)`, `log10(bact. load)`)
  
  tmp <- 
    shannon |> 
    inner_join(clin, by = join_by(USUBJID, AVISITN)) |>
    left_join(visits, by = join_by(AVISITN)) |> 
    mutate(key = str_c(USUBJID, "_", target_visit)) |> 
    select(-AVISITN, -target_visit, -USUBJID) |> 
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    bind_rows(
      tibble(variable = "α diversity (r)", descr = "Difference between the Shannon diversity index at the previous visit, computed on taxa proportions quantified from 16S rRNA gene sequencing data, and its expected value based on colonization category (as defined in the assay `endpoints`)."),
      tibble(variable = "pH (r)", descr = "Difference between vaginal pH at the previous visit, measured at study sites, and its expected value based on colonization category (as defined in the assay `endpoints`)."),
      tibble(variable = "log10(bact. load)", descr = "Log10 total bacterial load at the previous visit, quantified by qPRC.")
      
    ) |> 
    mutate(block = "Vag. env. pre-MTZ") |> select(block, everything())
  
  list(data = tmp, descr = descr)   
}

get_default_cytokine_r_assay <- function() "cytokine_residuals"

get_baseline_cytokines <- function(mae, PID, target_visits){
  tmp <- 
    get_assay_wide_format(mae, get_default_cytokine_r_assay()) |> 
    filter(USUBJID %in% PID, AVISITN == 0) |> 
    select(USUBJID, assay)
  tmp <- 
    expand_grid(USUBJID = unique(tmp$USUBJID), AVISITN = target_visits) |>
    mutate(key = str_c(USUBJID, "_", AVISITN)) |> select(-AVISITN) |> 
    inner_join(tmp, by = join_by(USUBJID)) |> 
    select(-USUBJID) |> 
    unnest(assay) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  colnames(tmp) <- str_c(colnames(tmp), " (r)")
  
  descr <- 
    tibble(
      block = "Cytokine (r) pre-MTZ", 
      descr = str_c("Pre-MTZ cytokine residual levels (see assay `", get_default_cytokine_r_assay(),"`). ",
                    "Values are computed as the difference between the transformed cytokine/chemokines levels ", 
                    "and their predicted value based on microbiota composition expressed as subcommunity proportions.")
    )
  
  list(data = tmp, descr = descr)  
}


get_prev_visit_cytokines <- function(mae, PID, target_visits){
  
  PIPV <- mae$AVISITN[mae$PIPV] |> unique(); PIPV <- PIPV[!is.na(PIPV)] |> sort()
  visits <- tibble(target_visit = PIPV) |> mutate(AVISITN = lag(target_visit)) |> 
    filter(target_visit %in% target_visits) 
  
  tmp <- 
    get_assay_wide_format(mae, get_default_cytokine_r_assay()) |> 
    filter(USUBJID %in% PID, AVISITN %in% visits$AVISITN) |> 
    select(USUBJID, AVISITN, assay) |> 
    left_join(visits, by = join_by(AVISITN)) |> 
    mutate(key = str_c(USUBJID, "_", target_visit)) |> 
    arrange(key) |> 
    select(-AVISITN, -target_visit, -USUBJID) |> 
    unnest(assay) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  colnames(tmp) <- str_c(colnames(tmp), " (r)")
  
  descr <- 
    tibble(
      block = "Cytokines (r) prev. v.", 
      descr = str_c("Cytokine residual levels at the previous visit (see assay `", get_default_cytokine_r_assay(),"`). ",
                    "Values are computed as the difference between the transformed cytokine/chemokines levels ", 
                    "and their predicted value based on microbiota composition expressed as subcommunity proportions.")
    )
  
  list(data = tmp, descr = descr)  
}



get_default_microbiota_assay <- function() "c_topics_16S_8"

get_baseline_microbiota <- function(mae, PID, target_visits, microbiota_assay){
  
  if(is.null(microbiota_assay)) microbiota_assay <- get_default_microbiota_assay()
  tmp <- 
    get_assay_wide_format(mae, microbiota_assay) |> 
    filter(USUBJID %in% PID, AVISITN == 0) |> 
    select(USUBJID, assay) |> 
    mutate(
      assay = 
        assay |> 
        set_colnames(
          SummarizedExperiment::rowData(mae[[get_default_microbiota_assay()]])$topic_label
          )
    )
      
  tmp <- 
    expand_grid(USUBJID = unique(tmp$USUBJID), AVISITN = target_visits) |>
    mutate(key = str_c(USUBJID, "_", AVISITN)) |> select(-AVISITN) |> 
    inner_join(tmp, by = join_by(USUBJID)) |> 
    select(-USUBJID) |> 
    unnest(assay) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    tibble(
      block = "Microbiota pre-MTZ", 
      descr = str_c("Pre-MTZ microbiota composition described as subcommunity proportions (see assay `", microbiota_assay,"`)")
    )
  
  list(data = tmp, descr = descr)  
}



get_prev_visit_category <- function(mae, PID, target_visits){
  
  PIPV <- mae$AVISITN[mae$PIPV] |> unique(); PIPV <- PIPV[!is.na(PIPV)] |> sort()
  visits <- tibble(target_visit = PIPV) |> mutate(AVISITN = lag(target_visit)) |> 
    filter(target_visit %in% target_visits) 
  
  tmp <- 
    get_assay_wide_format(mae, "mb_categories_wide") |> 
    filter(USUBJID %in% PID, AVISITN %in% visits$AVISITN) |> 
    select(USUBJID, AVISITN, assay) |> 
    left_join(visits, by = join_by(AVISITN)) |> 
    mutate(key = str_c(USUBJID, "_", target_visit)) |> 
    arrange(key) |> 
    select(-AVISITN, -target_visit, -USUBJID) |> 
    unnest(assay) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  descr <- 
    tibble(
      block = "Coloniz. cat. prev. v.", 
      descr = str_c("Colonization category at the previous visit. One of these mutually exclusive categories: `≥ 50% L. crispatus`, `≥ 50% Lactobacillus`, and `< 50% Lactobacillus`.")
    )
  
  list(data = tmp, descr = descr)  
}


get_prev_visit_microbiota <- function(mae, PID, target_visits, microbiota_assay){
  
  PIPV <- mae$AVISITN[mae$PIPV] |> unique(); PIPV <- PIPV[!is.na(PIPV)] |> sort()
  visits <- tibble(target_visit = PIPV) |> mutate(AVISITN = lag(target_visit)) |> 
    filter(target_visit %in% target_visits) 
  
  if(is.null(microbiota_assay)) microbiota_assay <- get_default_microbiota_assay()
  
  tmp <- 
    get_assay_wide_format(mae, microbiota_assay) |> 
    filter(USUBJID %in% PID, AVISITN %in% visits$AVISITN) |> 
    select(USUBJID, AVISITN, assay) |> 
    mutate(
      assay = 
        assay |> 
        set_colnames(
          SummarizedExperiment::rowData(mae[[get_default_microbiota_assay()]])$topic_label
        )
    ) |> 
    left_join(visits, by = join_by(AVISITN)) |> 
    mutate(key = str_c(USUBJID, "_", target_visit)) |> 
    arrange(key) |> 
    select(-AVISITN, -target_visit, -USUBJID) |> 
    unnest(assay) |>
    as.data.frame() |> 
    column_to_rownames("key")
  
  cat <- get_prev_visit_category(mae, PID, target_visits)$data |> as.matrix()
  cat <- t(cat) / colSums(cat)
  mean_props <- cat %*% as.matrix(tmp) 
  residuals <- tmp - mean_props[apply(cat, 2, which.max),]
  
  colnames(residuals) <- str_c(colnames(residuals), " (r)")

  descr <- 
    tibble(
      block = "Microbiota (r) prev.v.", 
      descr = str_c("Difference between the observed microbiota composition at the previous visit described as subcommunity proportions (see assay `", microbiota_assay,"`) and the average microbiota composition within each colonization category (as defined in the assay `endpoints`).")
    )
  
  list(data = residuals, descr = descr)  
}


get_demographics <- function(mae, PID, target_visits) {
  
  clin <- MultiAssayExperiment::colData(mae) |> as.data.frame() |> 
    mutate(N_PAST_BV = N_PAST_BV |> forcats::fct_relevel("Unknown",  after = 2L))

  variables <- c("USUBJID", "AVISITN", "AGE", "N_PAST_BV",  "RACEGR2", "EDULVL")
  
  dem_data <- 
    clin |> 
    select(all_of(variables)) |> 
    filter(AVISITN %in% target_visits, !is.na(USUBJID)) |>
    distinct() |> 
    dplyr::rename(`N past BV` = N_PAST_BV, Age = AGE, `Education level` = EDULVL) |> 
    mutate(
      `N past BV` = `N past BV` |>  as.integer(),
      `Education level` = `Education level` |>  as.integer()
    ) |> 
    arrange(RACEGR2) %>% 
    mutate(tmp = 1) %>% 
    tidyr::pivot_wider(
      names_from = RACEGR2, values_from = tmp, values_fill = 0,
      names_prefix = "Race: "
    ) |> 
    mutate(key = str_c(USUBJID, "_", AVISITN)) |> select(-USUBJID, -AVISITN) |> 
    arrange(key) |> 
    as.data.frame() |> 
    column_to_rownames("key")
    
  
  descr <- 
    bind_rows(
      tibble(variable = "Age", descr = "Age of the participant at enrollment."),
      tibble(variable = "N past BV", descr = str_c("Number of past BV episodes (coded as a number on the following scale: ", 
                                                   str_c(str_c("(", 1:nlevels(clin$EDULVL), ") ", levels(clin$N_PAST_BV)), collapse = ", "), ").")),
      tibble(variable = "Education level", descr = str_c("Education level of the participant (coded as a number on the following scale: ", 
                                                      str_c(str_c("(", 1:nlevels(clin$EDULVL), ") ", levels(clin$EDULVL)), collapse = ", "), ").")),
      tibble(variable = "Race", descr = 'Self-declared race of the participant (Categories with few participants were merged into "Other").')
      ) |> 
    mutate(block = "Demographics") |>
    select(block, everything())
  
  list(
    data = dem_data, 
    descr = descr
  )
}