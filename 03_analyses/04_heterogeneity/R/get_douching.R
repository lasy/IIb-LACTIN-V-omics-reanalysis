
get_douching <- function(clin, IDs, target_visit){
  clin %>% 
    filter(
      USUBJID %in% IDs,
      AVISITN <= target_visit
      ) %>% 
    select(USUBJID, AVISITN, contains("DOUCH")) %>% 
    mutate(
      SINCE_LV_DOUCH = SINCE_LV_DOUCH %>% replace_na(FALSE),
      CURR_DOUCH = CURR_DOUCH %>% replace_na(FALSE),
      LAST_DOUCH = LAST_DOUCH %>% replace_na(Inf)
      ) %>% 
    arrange(USUBJID, AVISITN) %>% 
    group_by(USUBJID) %>% 
    summarize(
      `N douching` = sum(SINCE_LV_DOUCH | CURR_DOUCH),
      `Recent douching` = any((AVISITN == target_visit) &  (LAST_DOUCH <= 7))*1
    ) %>% 
    arrange(USUBJID) %>% 
    select(-USUBJID) %>% 
    as.data.frame() %>% 
    set_rownames(sort(IDs))
}