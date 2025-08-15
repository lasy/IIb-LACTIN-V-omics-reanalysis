
get_postMTZ_load <- function(data_dir, IDs) {
  visits <- 
    readRDS(
      file = str_c(data_dir, "01_preprocessed/Clinical metadata/visits.RDS")
    )
  
  postMTL_load <- 
  visits %>% 
    filter(
      USUBJID %in% IDs,
      AVISITN %in% c(1, 2)
    ) %>% 
    group_by(AVISITN) %>% 
    mutate(
      load = log10(LOAD + 0.01),
      load = load %>% tidyr::replace_na(mean(load, na.rm = TRUE))
    ) %>% 
    ungroup() %>% 
    select(USUBJID, AVISITN, load) %>% 
    tidyr::pivot_wider(
      id_cols = USUBJID,
      names_from = AVISITN,
      values_from = load,
      names_prefix = "log10 load V"
    ) %>% 
    mutate(
      across(
        .cols = starts_with("log10 load V"), 
        function(x) tidyr::replace_na(x, mean(x, na.rm = TRUE))
      )
    ) %>% 
    arrange(USUBJID) %>% 
    select(-USUBJID) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
  
  colnames(postMTL_load) <- 
    colnames(postMTL_load) %>% 
    str_replace(., "V1", "post-MTZ") %>% 
    str_replace(., "V2", "week 4")
  
  postMTL_load
}