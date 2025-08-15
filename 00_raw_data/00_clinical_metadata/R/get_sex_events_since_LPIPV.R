
get_sex_events_since_LPIPV <- function(visits, events){
  
  
  sex_dict <- 
    tibble(
      VARIABLE = c("Sex without condoms", "Sex with condoms", "Sex (any)" ),
      var_code = c("UNPROT_SEX","PROT_SEX","SEX"),
      print_text = c("condomless", "condom-protected","any")
    )
  
  sex <- events %>% filter(CATEGORY == "Sexual behavior")
  sex_any <- 
    bind_rows(sex, sex |> mutate(VARIABLE = "Sex (any)")) |> 
    arrange(USUBJID, DAY)
  
  post_day_1_visits <- 
    visits |> 
    filter(AVISITN > 1) |> 
    select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV, NDAY_LPIPV) |> 
    rename(DAY_VISIT = DAY)
  
  
  # First, we compute the number of sexual intercourse since last planned in-person visit
  # (then, we will compute when the last sexual intercourse happened)
  
  sex_since_LPIPV <- 
    post_day_1_visits |> 
    full_join(sex_any, by = join_by(USUBJID), relationship = "many-to-many") |> 
    filter(DAY < DAY_VISIT, DAY >= DAY_LPIPV)  |> 
    group_by(USUBJID, AVISITN, NDAY_LPIPV, VARIABLE) |> 
    summarize(N_SINCE_LPIPV = n(), .groups = "drop") |> 
    mutate(R_SINCE_LPIPV = N_SINCE_LPIPV/NDAY_LPIPV) |> 
    select(-NDAY_LPIPV)
  
  # We pivot wider so that each type of sex has their own columns
  sex_since_LPIPV_wide <- 
    sex_since_LPIPV |> 
    left_join(sex_dict |> select(VARIABLE, var_code), by = join_by(VARIABLE)) |> 
    select(-VARIABLE) |> 
    pivot_wider(names_from = var_code, values_from = contains("SINCE_LPIPV"), values_fill = 0)
  # and we make sure we have one value for each visit
  sex_since_LPIPV_wide <- 
    sex_since_LPIPV_wide |> 
    right_join(visits |> select(USUBJID, AVISITN), by = join_by(USUBJID, AVISITN)) |> 
    arrange(USUBJID, AVISITN) |>
    mutate(across(contains("_SINCE_LPIPV"), ~ifelse((AVISITN > 1) & is.na(.x), 0, .x)))
  
  # Then, we compute the number of days since the last sexual intercourse, regardless of whether that was before LPIPV or not
  
  last_sex <- 
    post_day_1_visits |> 
    full_join(sex_any, by = join_by(USUBJID), relationship = "many-to-many") |> 
    filter(DAY < DAY_VISIT)  |>
    group_by(USUBJID, AVISITN, DAY_VISIT, VARIABLE) |> 
    summarize(LAST = DAY_VISIT[1] - max(DAY), .groups = "drop") |> 
    select(-DAY_VISIT) |> 
    # we pivot wider using the variable code
    left_join(sex_dict |> select(VARIABLE, var_code), by = join_by(VARIABLE)) |> 
    select(-VARIABLE) |>
    pivot_wider(names_from = var_code, values_from = LAST, names_prefix = "LAST_") |> 
    # we make sure we have one row per visit
    right_join(visits |> select(USUBJID, AVISITN, DAY), by = join_by(USUBJID, AVISITN)) |> 
    arrange(USUBJID, AVISITN) |>
    mutate(across(contains("LAST"), ~ifelse((AVISITN > 1) & is.na(.x), DAY + 15, .x))) |> 
    select(-DAY)
  
  sex_summary <- left_join(sex_since_LPIPV_wide, last_sex, by = join_by(USUBJID, AVISITN))
     
  var_info <-
    expand_grid(
      tibble(
        var = c("N_SINCE_LPIPV", "R_SINCE_LPIPV", "LAST"),
        print_1 = c("Nb of days with", "Daily rate of", "Nb days since"),
        print_2 = c(rep("vag. sex since last planned in-person visit",2), "vag. sex")
      ),
      sex_dict
    ) |> 
    mutate(
      var = str_c(var, "_", var_code),
      label = str_c(print_1, " ", print_text , " ", print_2)
    ) |>
    select(var, label) |>
    mutate(
      type = "integer",
      group = "Sexual behavior"
    )
  
  list(
    summary = sex_summary,
    variable_info = var_info
  )
  
}




# sex_since_LPIPV <- 
#   visits |> 
#   filter(AVISITN > 1) |> 
#   select(USUBJID, AVISITN, DAY, LPIPV, DAY_LPIPV, NDAY_LPIPV) |> 
#   rename(DAY_VISIT = DAY) |> 
#   full_join(sex_any, by = join_by(USUBJID), relationship = "many-to-many") |> 
#   filter(DAY < DAY_VISIT, DAY >= DAY_LPIPV)  |> 
#   group_by(USUBJID, AVISITN, DAY_VISIT, NDAY_LPIPV, VARIABLE) |> 
#   summarize(N_SINCE_LPIPV = n(), DAY_LAST = max(DAY), .groups = "drop") |> 
#   mutate(
#     LAST = DAY_VISIT - DAY_LAST, 
#     R_SINCE_LPIPV = N_SINCE_LPIPV/NDAY_LPIPV
#   ) |> 
#   select(-DAY_LAST, -DAY_VISIT, -NDAY_LPIPV)
# 
# 
# # We pivot wider so that each type of sex has their own columns
# 
# sex_since_LPIPV_wide <- 
#   sex_since_LPIPV |> 
#   left_join(sex_dict |> select(VARIABLE, var_code), by = join_by(VARIABLE)) |> 
#   select(-VARIABLE) |> 
#   pivot_wider(
#     names_from = var_code, values_from = c(N_SINCE_LPIPV, R_SINCE_LPIPV, LAST),
#     values_fill = list(N_SINCE_LPIPV = 0, R_SINCE_LPIPV = 0, LAST = NA)
#   ) |> 
#   right_join(visits |> select(USUBJID, AVISITN), by = join_by(USUBJID, AVISITN)) |> 
#   arrange(USUBJID, AVISITN) |>
#   mutate(
#     across(
#       contains("_SINCE_LPIPV"), 
#       ~ifelse((AVISITN > 1) & is.na(.x), 0, .x)
#     )
#   )

