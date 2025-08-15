
check_associations_with_sites <- function(clin, target_visit, variable){
  filtered_clin <- 
    clin %>% 
    filter(USUBJID %in% get_included_participants(clin, target_visit)$USUBJID) %>% 
    select(USUBJID, SITENAME, all_of(variable)) %>% 
    distinct()
  
  var_table <- table(Site = filtered_clin$SITENAME %>% fct_drop(), group = filtered_clin[,variable])
  Xsq <- chisq.test(var_table, simulate.p.value = TRUE)
  
  var_table %>% 
    as_tibble() %>% 
    group_by(Site) %>% 
    mutate(
      total = sum(n),
      perc = (100 * n / total) %>% round(),
      res = str_c(n, " (",perc,"%)")
    ) %>% 
    pivot_wider(id_cols = c(Site, total), names_from = group, values_from = res) %>% 
    kableExtra::kable(
      bookdown = TRUE, 
      caption = str_c("'", variable, "' groups per study site. Chi-squared test p-value < ", ceiling(1000*Xsq$p.value)/1000)
    ) %>% 
    kableExtra::kable_styling(latex_options = c(position = "H"))
  
} 