

get_sexual_behavior_since_LPIPV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  tmp <- 
    MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(
      Barcode, 
      R_SINCE_LPIPV_PROT_SEX, R_SINCE_LPIPV_UNPROT_SEX, 
      LAST_PROT_SEX, LAST_UNPROT_SEX, 
      N_NEW_PARTNERS
    ) %>% 
    mutate(
      `Avg. rate prot. sex` = R_SINCE_LPIPV_PROT_SEX,
      `Avg. rate unprot. sex` = R_SINCE_LPIPV_UNPROT_SEX,
      `Days since last prot. sex` = LAST_PROT_SEX,
      `Days since last unprot. sex` = LAST_UNPROT_SEX,
      `Nb new partners` = N_NEW_PARTNERS
    ) %>% 
    select(Barcode,  matches("[a-z]$", ignore.case = FALSE)) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
  tmp
  
}

get_sexual_behavior_throughout <- function(mae, Barcodes, target_visit){
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  
  weeks <- c("W1","W2_4","W5_8","W9_12")
  if (target_visit == 7) weeks <- c(weeks,"W13_24")
  S_colnames <- 
    expand_grid(sex_type = c("CNDM_SEX", "CNDMLESS_SEX"), weeks = weeks) |> 
    mutate(col = str_c(sex_type, "_", weeks))
  
  S_colnames <- c(S_colnames$col, "N_NEW_PARTNERS_W1_12")

  res <- 
  MultiAssayExperiment::colData(mae) |> 
    as.data.frame() |> 
    filter(Barcode %in% Barcodes) |> 
    arrange(Barcode) |> 
    select(all_of(S_colnames))
  
  colnames(res) <- 
    colnames(res) |> 
    str_replace_all("N_NEW_PARTNERS_W1_12", "Nb new partners") |>
    str_replace_all("CNDM", "Condom") |> 
    str_replace_all("_SEX_", " sex " ) |> 
    str_replace_all("_", " to ") |> 
    str_to_sentence()
  
  res
  
}


# Deprecated




get_sexual_behavior <- function(data_dir, clin, IDs, target_visit) {
  
  if (target_visit == 4) max_DAY <- 84 else max_DAY <- Inf
  
  events <- 
    readRDS(
      file = str_c(data_dir, "01_preprocessed/Clinical metadata/events.RDS")
    )
  
  sexual_behavior <- 
    events %>% 
    filter(
      USUBJID %in% IDs,
      str_detect(VARIABLE, "Sex"),
      DAY <= max_DAY
    ) %>%
    mutate(
      week_group  = 
        case_when(
          DAY %in% c(1:7) ~ "W1",
          DAY %in% c(8:28) ~ "W2-4",
          DAY %in% c(29:56) ~ "W5-8", 
          DAY %in% c(57:84) ~ "W9-12",
          DAY >= 85 ~ "W13-24"
        )
    ) %>% 
    group_by(USUBJID, VARIABLE, week_group) %>% 
    summarize(n_sex = n(), .groups = "drop") %>% 
    full_join(tibble(USUBJID = IDs), by = "USUBJID") %>% 
    mutate(
      week_group = week_group %>% str_replace_na("W1"),
      n_sex = n_sex %>% tidyr::replace_na(0),
      VARIABLE = 
        ifelse(
          VARIABLE == "Sex with condoms", 
          "Condom sex", 
          "No condoms sex"
        ) %>% 
        str_replace_na("Condom sex")
    ) %>% 
    arrange(VARIABLE, week_group) %>% 
    tidyr::pivot_wider(
      id_cols = USUBJID,
      names_from = c(VARIABLE, week_group),
      values_from = n_sex,
      values_fill = 0,
      names_sep = " "
    )
  
  new_partners <- 
    clin %>% 
    filter(USUBJID %in% IDs, AVISITN > 1, AVISITN <= target_visit) %>% 
    select(USUBJID, AVISITN, N_NEW_PARTNERS) %>% 
    mutate(visit = ceiling(AVISITN))  %>% 
    group_by(USUBJID, visit) %>% 
    summarize(
      N_NEW_PARTNERS = N_NEW_PARTNERS %>% sum(., na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    pivot_wider(
      id_cols = USUBJID, names_from = visit, values_from = N_NEW_PARTNERS,
      names_prefix = "New partners V"
    ) %>% 
    mutate(across(starts_with("New partners"), function(x) replace_na(x, 0)))
  
  recent_sex <- 
    clin %>% 
    filter(USUBJID %in% IDs, AVISITN == target_visit) %>% 
    select(USUBJID, AVISITN, LAST_PROT_SEX, LAST_UNPROT_SEX) %>% 
    mutate(
      LAST_PROT_SEX = LAST_PROT_SEX %>% replace_na(Inf),
      LAST_UNPROT_SEX = LAST_UNPROT_SEX %>% replace_na(Inf),
      `Recent protected sex` = (LAST_PROT_SEX <= 7)*1,
      `Recent unprotected sex` = (LAST_UNPROT_SEX <= 7)*1
    ) %>% 
    select(USUBJID,  `Recent protected sex` , `Recent unprotected sex` )
    
  sexual_behavior <- 
    sexual_behavior %>% 
    left_join(new_partners, by = join_by(USUBJID)) %>% 
    left_join(recent_sex, by = join_by(USUBJID)) %>% 
    arrange(USUBJID) %>% 
    select(-USUBJID) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
  
  
  
  
  simple_sex <- 
    sexual_behavior %>% 
    mutate(
      `Protected sex` = 
        `Condom sex W1` + `Condom sex W2-4` + 
        `Condom sex W5-8` + `Condom sex W9-12`,
      `Unprotected sex` = 
        `No condoms sex W1` + `No condoms sex W2-4` + 
        `No condoms sex W5-8` + `No condoms sex W9-12`,
      `New partners` = `New partners V2` + `New partners V3` + `New partners V4`
    ) %>% 
    select(
      `Protected sex`, `Unprotected sex`, `New partners`, 
      `Recent protected sex`, `Recent unprotected sex`
    )
  
  list(detailed = sexual_behavior, summary = simple_sex)
}



get_sexual_behavior_since_LV <- function(mae, Barcodes){
  
  if (!all(Barcodes %in% mae$Barcode)) stop("all `Barcodes` must be present in mae$Barcode.")
  
  MultiAssayExperiment::colData(mae) %>% 
    as.data.frame() %>% 
    filter(Barcode %in% Barcodes) %>% 
    select(Barcode, N_PROT_SEX, N_UNPROT_SEX, LAST_PROT_SEX, LAST_UNPROT_SEX, N_NEW_PARTNERS) %>% 
    mutate(
      `# prot. sex` = N_PROT_SEX %>% replace_na(median(N_PROT_SEX, na.rm = TRUE)),
      `# unprot. sex` = N_UNPROT_SEX %>% replace_na(median(N_UNPROT_SEX, na.rm = TRUE)),
      `Days since last prot. sex` = LAST_PROT_SEX %>% replace_na(median(LAST_PROT_SEX, na.rm = TRUE)),
      `Days since last unprot. sex` = LAST_UNPROT_SEX %>% replace_na(median(LAST_UNPROT_SEX, na.rm = TRUE)),
      `# new partners` = N_NEW_PARTNERS %>% replace_na(median(N_NEW_PARTNERS, na.rm = TRUE))
    ) %>% 
    select(Barcode,  `# prot. sex`, `# unprot. sex`,  
           `Days since last prot. sex`, `Days since last unprot. sex`,
           `# new partners`) %>% 
    as.data.frame() %>% 
    set_rownames(NULL) %>% 
    arrange(Barcode) %>% 
    column_to_rownames("Barcode")
  
}


