
get_BC_summary <- function(visits, events){
  
  var_info <- 
    bind_rows(
      tibble(var = 'BC', 
             label = "Current birth control" , 
             type = "character", 
             group = "Birth control"),
      tibble(var = 'BC_FULL', 
             label = "Current birth control (details)" , 
             type = "character", 
             group = "Birth control"),
      tibble(var = 'BC_CHANGE', 
             label = "Has changed B.C. since last planned in-person visit" , 
             type = "logical", 
             group = "Birth control")
    )
  
  BC_events <- 
    events %>% 
    filter(CATEGORY == "Birth control") %>% 
    select(USUBJID, DAY, VARIABLE, VALUE) %>% 
    rename(BC = VARIABLE, BC_detail = VALUE) 
  
  
  BC_levels <- 
    c("Combined","P only","IUD (Non-hormonal)","IUD (Hormonal)", "Non-hormonal")
  
  summary_BC <- 
    visits |> 
    select(USUBJID, AVISITN, DAY) |>
    # It is not uncommon for participants to use several birth control at the same time. This is why we have the option "many-to-many"
    left_join(BC_events, by = c("USUBJID", "DAY"), relationship = "many-to-many") |> 
    # For downstream analyses, it makes sense to drop the "non-hormonal" methods that are used in addition to other hormonal methods or IUDs. 
    mutate(BC = BC |> factor(levels = BC_levels)) |>
    arrange(USUBJID, AVISITN, DAY, BC) |>
    group_by(USUBJID, AVISITN, DAY) |> 
    summarize(
      BC = BC[1],
      BC_FULL = str_c(BC_detail |> unique() |> sort(), collapse = " | "),
      .groups = "drop"
    ) |> 
    mutate(BC = BC |> fct_expand("unknown") |> replace_na("unknown"))
  
  # Now, we also add whether there was a change in primary BC since the last PIPV
  
  summary_BC <- 
    summary_BC |> 
    left_join(
      visits |> select(USUBJID, AVISITN, LPIPV), by = c("USUBJID", "AVISITN")
    ) |>
    left_join(
      summary_BC |> 
        select(USUBJID, AVISITN, BC) |> 
        rename(LPIPV = AVISITN, BC_LPIPV = BC),
      by = join_by(USUBJID, LPIPV)
    ) |> 
    mutate(BC_CHANGE = (BC != BC_LPIPV)) |> 
    select(-LPIPV, -BC_LPIPV, -DAY)
  
  list(summary = summary_BC, variable_info = var_info)
  
}
