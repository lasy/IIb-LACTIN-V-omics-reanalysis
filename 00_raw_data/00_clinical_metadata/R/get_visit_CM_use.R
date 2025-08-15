

get_visit_CM_use_LPIPV <- function(visits, events){
  
  CM_dict_ <-
    bind_rows(
      tibble(VARIABLE = "Vaginal antibiotics", code = "V_ABIO"),
      tibble(VARIABLE = "Oral antibiotics", code = "O_ABIO"),
      tibble(VARIABLE = "Douching", code = "DOUCH"),
      tibble(VARIABLE = "Steroids", code = "STER"),
      tibble(VARIABLE = "Antifungal", code = "AFUN")
    )
  
  tmp <-
    map(
      .x = CM_dict_$VARIABLE,
      .f = get_visit_CM_use_VAR_LPIPV,
      visits = visits, events = events,
      CM_dict_ = CM_dict_
    ) |> 
    bind_rows()
  
  tmp_wide <-
    pivot_wider(
      tmp,
      id_cols = c(USUBJID, AVISITN),
      names_from = VARIABLE,
      values_from = c(N_SINCE_LPIPV, LAST, CURR)
    )
  
  
  var_info <-
    bind_rows(
      tibble(var = 'CURR',
             label = "is currently using " ,
             type = "logical"),
      tibble(var = 'N_SINCE_LPIPV',
             label = " was used since last planned in-person visit" ,
             type = "logical"),
      tibble(var = 'LAST',
             label = "Days since last " ,
             type = "integer")
    ) %>%
    mutate(group = "CM") %>%
    expand_grid(., CM_dict_) %>%
    mutate(
      label =
        case_when(
          var == "N_SINCE_LPIPV" ~ str_c(VARIABLE, label),
          TRUE ~ str_c(label, VARIABLE %>% str_to_lower())
        ),
      var = str_c(var, "_", code)
    ) |> 
    select(var, label, type, group)
  
  list(
    CM_use = tmp_wide,
    variable_info = var_info
  )
  
}

get_visit_CM_use_VAR_LPIPV <- function(VAR, visits, events, CM_dict_){
  
  var_events <- 
    events |> 
    filter(CATEGORY == "Concomitant medication", VARIABLE == VAR) |> 
    select(USUBJID, DAY, VARIABLE)
  
  tmp_visits <- 
    visits |> 
    filter(AVISITN >= 2) |> 
    select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV) |> 
    rename(DAY_VISIT = DAY)
  
  tmp <- 
    full_join(tmp_visits, var_events, by = join_by(USUBJID), relationship = "many-to-many") |> 
    filter(DAY >= DAY_LPIPV, DAY < DAY_VISIT)  |> 
    arrange(USUBJID, DAY) |> 
    group_by(USUBJID, AVISITN, DAY_VISIT) |> 
    summarize(N_SINCE_LPIPV = n(), DAY_LAST = max(DAY), .groups = "drop") |> 
    mutate(LAST = DAY_VISIT - DAY_LAST, CURR = ((DAY_LAST + 1) == DAY_VISIT)) |> 
    select(-DAY_LAST, -DAY_VISIT) |> 
    right_join(visits |> select(USUBJID, AVISITN), by = join_by(USUBJID, AVISITN)) |> 
    arrange(USUBJID, AVISITN) |> 
    mutate(
      N_SINCE_LPIPV = N_SINCE_LPIPV |> replace_na(0),
      CURR = CURR |> replace_na(FALSE)
      ) |> 
    mutate(VARIABLE = CM_dict_$code[CM_dict_$VARIABLE == VAR]) %>%
    select(VARIABLE, everything())

  tmp
}


get_visit_CM_use <- function(visits, events){

  CM_dict_ <-
    bind_rows(
    tibble(VARIABLE = "Vaginal antibiotics", code = "V_ABIO"),
    tibble(VARIABLE = "Oral antibiotics", code = "O_ABIO"),
    tibble(VARIABLE = "Douching", code = "DOUCH"),
    tibble(VARIABLE = "Steroid", code = "STER"),
    tibble(VARIABLE = "Antifungal", code = "AFUN")
        )

  tmp <-
    purrr::map_dfr(
      .x = CM_dict_$VARIABLE,
      .f = get_visit_CM_use_VAR,
      visits = visits, events = events,
      CM_dict_ = CM_dict_
    )

  tmp_wide <-
    pivot_wider(
      tmp,
      id_cols = c(USUBJID, AVISITN, DAY),
      names_from = VARIABLE,
      values_from = c(SINCE_LV, LAST, CURR)
    )


  variable_info <-
    bind_rows(
      tibble(var_name = 'CURR',
             var_name_print = "is currently using " ,
             var_type = "logical"),
      tibble(var_name = 'SINCE_LV',
             var_name_print = " was used since last visit" ,
             var_type = "logical"),
      tibble(var_name = 'LAST',
             var_name_print = "days since last " ,
             var_type = "integer")
    ) %>%
    mutate(var_group = "CM") %>%
    expand_grid(., CM_dict_) %>%
    mutate(
      var_name_print =
        case_when(
          var_name == "SINCE_LV" ~ str_c(VARIABLE, var_name_print),
          TRUE ~ str_c(var_name_print, VARIABLE %>% str_to_lower())
        ),
      var_name = str_c(var_name, "_", code)
      )

  list(
    CM_use = tmp_wide,
    variable_info = variable_info
  )

}

get_visit_CM_use_VAR  <- function(VAR, visits, events, CM_dict_){

  tmp <-
    bind_rows(
      visits %>%  filter(AVISITN > 1) %>%
        select(USUBJID, AVISITN, DAY) %>%
        mutate(VARIABLE = "Visit"),
      events %>%
        filter(CATEGORY == "Concomitant medication", VARIABLE == VAR) %>%
        select(USUBJID, DAY, VARIABLE)
    ) %>%
    mutate(VARIABLE = VARIABLE %>% factor(., levels = c("Visit",VAR))) %>%
    arrange(USUBJID, DAY, VARIABLE) %>%
    group_by(USUBJID) %>%
    mutate(
      SINCE_LV = (VARIABLE == "Visit") & (lag(VARIABLE) == VAR),
      LAST = ifelse(SINCE_LV, DAY - lag(DAY), NA),
      CURR = (LAST == 0)
    ) %>%
    filter(VARIABLE == "Visit") %>%
    select(-VARIABLE) %>%
    mutate(VARIABLE = CM_dict_$code[CM_dict_$VARIABLE == VAR]) %>%
    select(VARIABLE, everything())

  tmp
}

