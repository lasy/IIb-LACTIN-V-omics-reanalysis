

get_doses_since_LPIPV <- function(visits, events){
  doses <- events %>% filter(CATEGORY == "Doses")
  
  doses_since_LPIPV <- 
    visits |> 
    filter(AVISITN > 1) |> 
    select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV) |> 
    rename(DAY_VISIT = DAY) |> 
    full_join(doses, by = join_by(USUBJID), relationship = "many-to-many") |> 
    filter(DAY < DAY_VISIT, DAY >= DAY_LPIPV)  |> 
    group_by(USUBJID, AVISITN) |> 
    summarize(N_DOSES_LPIPV = n(), .groups = "drop") |> 
    right_join(visits |> select(USUBJID, AVISITN), by = join_by(USUBJID, AVISITN)) |> 
    arrange(USUBJID, AVISITN) |> 
    mutate(N_DOSES_LPIPV = ifelse((AVISITN > 1) & is.na(N_DOSES_LPIPV), 0, N_DOSES_LPIPV))
  
  PP_doses_since_LPIPV <- 
    visits |> 
    filter(AVISITN > 1) |> 
    select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV) |> 
    rename(DAY_VISIT = DAY) |> 
    mutate(
      PP_DOSES_VISIT = get_PP_doses(DAY_VISIT),
      PP_DOSES_LPIPV = get_PP_doses(DAY_LPIPV),
      PP_DOSES_LPIPV = ifelse(LPIPV == 1, 0, PP_DOSES_LPIPV),
      N_PP_DOSES_LPIPV = PP_DOSES_VISIT - PP_DOSES_LPIPV
    ) |> 
    select(USUBJID, AVISITN, N_PP_DOSES_LPIPV) 
  
  doses_since_LPIPV <- 
    doses_since_LPIPV |> 
    left_join(PP_doses_since_LPIPV, by = join_by(USUBJID, AVISITN))
  
  var_info <-
    bind_rows(
      tibble(var = "N_DOSES_LPIPV", label = "Nb of doses since last planned in-person visit"),
      tibble(var = "N_PP_DOSES_LPIPV", label = "Expected nb of doses since last planned in-person visit (per-protocol)")
    ) %>%
    mutate(
      type = "integer",
      group = "Doses"
    )
  
  list(
    summary = doses_since_LPIPV,
    variable_info = var_info
  )
  
}

get_PP_doses <- function(DAY){
  PP_doses_days <- c(1:5, 1 + 7 * (1:10), 5 + 7 * (1:10) ) |> sort()
  map(DAY, function(x) sum(PP_doses_days < x)) |> as.integer()
}


get_doses_since_last_visit <- function(visits, events){
  doses <- events %>% filter(CATEGORY == "Doses")
  tmp <- 
    visits %>% 
    select(USUBJID, AVISITN, DAY, VISIT_TYPE) %>% 
    filter(AVISITN %in% c(1:4,7)) %>% 
    arrange(USUBJID, AVISITN) %>% 
    group_by(USUBJID) %>% 
    mutate(days_since_last_visit = DAY - lag(DAY)) %>% 
    ungroup() %>% 
    filter(!is.na(days_since_last_visit))
  visits_days <- tmp[rep(1:nrow(tmp), tmp$days_since_last_visit),1:3]
  visits_days <-  
    visits_days %>% 
    mutate(VISIT_DAY = DAY) %>% 
    group_by(USUBJID) %>% 
    mutate(DAY = row_number()) %>% 
    ungroup()
  
  doses_with_visits <- 
    doses %>% full_join(visits_days, by = join_by(USUBJID, DAY))
  
  summary <- 
    doses_with_visits %>% 
    group_by(USUBJID, AVISITN) %>% 
    summarize(N_DOSES_LV = sum(!is.na(CATEGORY)), .groups = "drop") %>% 
    filter(!is.na(AVISITN))
  
  
  variable_info <-
    bind_rows(
      tibble(var_name = "N_DOSES_LV", var_name_print = "Nb of doses since last planned visit")
    ) %>%
    mutate(
      var_type = "integer",
      var_group = "Doses"
    )
  
  
  list(
    summary = summary,
    variable_info = variable_info
  )
  
}
