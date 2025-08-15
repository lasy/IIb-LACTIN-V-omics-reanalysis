
get_menstruation_summary <- function(visits, events){
  var_info <-  
    bind_rows(
      tibble(var = 'MENSES', 
             label = "is menstruating", 
             type = "logical"),
      tibble(var = 'LAST_M', 
             label = "days since last menstrual bleeding",
             type = "integer"),
      tibble(var = 'N_BLEED_LPIPV', 
             label = "number of days bleeding since last planned in-person visit",
             type = "integer")
    ) |> 
    mutate(group = "Menstruation")
  
  
  menstruation_events <- 
    events |> 
    filter(CATEGORY == "Menstruation") |> 
    select(USUBJID, DAY) |> 
    mutate(MENSES = TRUE)
  
  menstruating_at_visit <- 
    left_join(
      visits |> select(USUBJID, AVISITN, DAY),
      menstruation_events,
      by = c("USUBJID", "DAY")
    ) |>
    mutate(MENSES = MENSES %>% replace_na(FALSE))
    
  days_since_last_bleeding <- 
    full_join(
      menstruation_events,
      visits |> select(USUBJID, AVISITN, DAY) |> rename(DAY_VISIT = DAY),
      by = join_by(USUBJID), relationship = "many-to-many"
    ) |> 
    filter(DAY <= DAY_VISIT) |> 
    group_by(USUBJID, AVISITN, DAY_VISIT) |> 
    summarize(DAY_LAST_M = max(DAY), .groups = "drop") |>
    mutate(LAST_M = DAY_VISIT - DAY_LAST_M) |> 
    select(USUBJID, AVISITN, LAST_M)
  
  n_days_bleeding_since_last_PIPV <- 
    full_join(
      menstruation_events,
      visits |> 
        select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV) |> 
        rename(DAY_VISIT = DAY),
      by = join_by(USUBJID),
      relationship = "many-to-many"
    ) |> 
    filter(DAY >= DAY_LPIPV, DAY < DAY_VISIT) |>
    arrange(USUBJID, AVISITN) |> 
    group_by(USUBJID, AVISITN) |>
    summarize(N_BLEED_LPIPV = n(), .groups = "drop") 
    
  
  summary <- 
    menstruating_at_visit |> 
    left_join(days_since_last_bleeding, by = join_by(USUBJID, AVISITN)) |> 
    mutate(LAST_M = ifelse((AVISITN > 1) & is.na(LAST_M), DAY + 15, LAST_M)) |>
    select(-DAY) |> 
    left_join(n_days_bleeding_since_last_PIPV, by = join_by(USUBJID, AVISITN)) |> 
    mutate(N_BLEED_LPIPV = ifelse((AVISITN > 1) & is.na(N_BLEED_LPIPV), 0, N_BLEED_LPIPV))
  
  list(summary = summary, variable_info = var_info)
}





# tmp <- 
#   full_join(
#     visits |> select(USUBJID, AVISITN, DAY),
#     menstruation_events,
#     by = c("USUBJID", "DAY")
#   ) |> 
#   #  %>% 
#   arrange(USUBJID, DAY, MENSES) %>% 
#   group_by(USUBJID) %>% 
#   mutate(visit_id = cumsum(!is.na(AVISITN))) %>% 
#   group_by(USUBJID) %>%
#   mutate(
#     DAY_LAST_M = ifelse(lag(MENSES), lag(DAY), NA),
#     DAY_LAST_M = ifelse(!is.na(MENSES) & MENSES, DAY, DAY_LAST_M),
#     LAST_M = DAY - DAY_LAST_M
#   ) %>% # 
#   filter(!is.na(AVISITN)) %>% 
#   select(USUBJID, AVISITN, MENSES, LAST_M) %>% 
#   mutate(MENSES = MENSES %>% replace_na(FALSE))