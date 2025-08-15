
get_treatment_arm <- function(clin, IDs) {
  clin %>% 
    filter(USUBJID %in% IDs) %>% 
    select(USUBJID, ARM) %>% 
    distinct() %>% 
    mutate(tmp = 1) %>% 
    pivot_wider(
      id_cols = USUBJID, names_from = ARM, values_from = tmp, values_fill = 0
      ) %>% 
    arrange(USUBJID)  %>% 
    as.data.frame() %>% 
    set_rownames(IDs %>% sort()) %>% 
    select(-USUBJID)
  
}
