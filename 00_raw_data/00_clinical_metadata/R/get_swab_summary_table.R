get_swab_summary_table <- function(visits, subjects) {

  criteria_dict <- 
    bind_rows(
      tibble(var_name = "exists", Criteria = "All participants (should be 152 - 76 as in NEJM, 2020)"),
      tibble(var_name = "any_swabs", Criteria = "Has at least one swab at any visit (including supp. visits)"),
      tibble(var_name = "any_swabs_at_PIPV", Criteria = "Has at least one swab at any planned in-person visit"),
      tibble(var_name = "one_swab_4", Criteria = "Has at least one swab at week 12 visit (primary endpoint visit)"),
      tibble(var_name = "one_swab_7", Criteria = "Has at least one swab at week 24 visit"),
      tibble(var_name = "one_swab_04", Criteria = "Has at least one swab at both pre-MTZ and week 12 visits"),
      tibble(var_name = "one_swab_014", Criteria = "Has at least one swab at pre-MTZ, post-MTZ, and week 12 visits"),
      tibble(var_name = "one_swab_01234", Criteria = "Has at least one swab at pre-MTZ, post-MTZ, and week 4, 8, and 12 visits"),
      tibble(var_name = "one_swab_012347", Criteria = "Has at least one swab at all planned in-person visits (pre-MTZ, post-MTZ, week 4, 8, 12, and 24)")
    )

  tmp_participants <- 
    visits |> 
    full_join(expand_grid(USUBJID = unique(visits$USUBJID), AVISITN = c(0:4,7)), by = join_by(USUBJID, AVISITN)) |> 
    select(USUBJID, AVISITN, PIPV, N_SWABS) |> 
    mutate(N_SWABS = N_SWABS |> replace_na(0)) |>
    left_join(subjects |> select(USUBJID, ARM), by = join_by(USUBJID)) |> 
    group_by(USUBJID, ARM) |>
    summarize(
      exists = TRUE,
      any_swabs = any(N_SWABS > 0),
      any_swabs_at_PIPV = any(N_SWABS[PIPV] > 0),
      one_swab_4 = all(N_SWABS[AVISITN %in% c(4)] > 0),
      one_swab_7 = all(N_SWABS[AVISITN %in% c(7)] > 0),
      one_swab_04 = all(N_SWABS[AVISITN %in% c(0,4)] > 0),
      one_swab_014 = all(N_SWABS[AVISITN %in% c(0,1,4)] > 0),
      one_swab_01234 = all(N_SWABS[AVISITN %in% c(0:4)] > 0),
      one_swab_012347 = all(N_SWABS[PIPV] > 0),
      .groups = "drop"
    )
  
  tmp_participants |> 
    pivot_longer(cols = -c(USUBJID, ARM), names_to = "var_name", values_to = "meet_criteria") |>
    filter(meet_criteria) |> 
    count(var_name, ARM) |> 
    pivot_wider(id_cols = var_name, names_from = ARM, values_from = n) |>
    left_join(criteria_dict, by = join_by(var_name)) |> 
    mutate(var_name = var_name |> factor(levels = criteria_dict$var_name)) |> 
    arrange(var_name) |> 
    select(-var_name) |> 
    select(Criteria, everything()) 

}


get_swab_summary_table_v1 <- function(visits, subjects) {
  
tmp <-
  visits %>%
  select(USUBJID, AVISITN, N_SWABS) %>%
  filter(AVISITN %in% c(0:4,7)) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = AVISITN,
    values_from = N_SWABS,
    names_prefix = "V",
    values_fill = 0
  ) %>%
  mutate(
    all_participants = TRUE,
    at_least_one_swab_case_0 = (V0 > 0) | (V1 > 0) | (V2 > 0) | (V3 > 0) | (V4 > 0) | (V7 > 0),
    at_least_one_swab_case_1 = (V0 > 0) & (V1 > 0) & (V4 > 0),
    at_least_one_swab_case_2 = at_least_one_swab_case_1 & (V2 > 0) & (V3 > 0),
    at_least_one_swab_case_3 = at_least_one_swab_case_2 & (V7 > 0)
  ) %>%
  left_join(
    subjects %>% select(USUBJID, ARM), by = "USUBJID"
  ) %>%
  pivot_longer(
    cols = c("all_participants", starts_with("at_least_one_swab")),
    names_to = "condition",
    values_to = "meet_criteria"
  ) %>%
  mutate(
    condition =
      case_when(
        str_detect(condition, "all_participants") ~ "all participants (should be 152 - 76 as in NEJM, 2020)",
        str_detect(condition, "case_0") ~ "at least one swab at any planned in-person visit",
        str_detect(condition, "case_1") ~ "at least one swab at pre-MTZ, post-MTZ, and week 12 visits",
        str_detect(condition, "case_2") ~ "at least one swab at pre-MTZ, post-MTZ, week 4, week 8, and week 12 visits",
        str_detect(condition, "case_3") ~ "at least one swab at pre-MTZ, post-MTZ, week 4, week 8, week 12 and week 24 visits",
      )
  )

tmp %>%
  filter(meet_criteria) %>%
  group_by(condition, ARM) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(id_cols = condition,
              names_from = ARM, values_from = n)

}
