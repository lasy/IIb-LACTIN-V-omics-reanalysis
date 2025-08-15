
get_visit_dates <- function(table_name){


  ADSX <- readxl::read_xlsx(stringr::str_c(ADaM_dir, "ADSX.xlsx"), guess_max = 10000)

  tmp <-
    ADSX %>%
    select(AVISITN, AVISIT) %>%
    distinct() %>%
    arrange(AVISITN) %>%
    mutate(visit_category = floor(AVISITN)) %>%
    group_by(visit_category) %>%
    mutate(VISIT = AVISIT[1]) %>%
    ungroup() %>%
    mutate(VISIT = VISIT %>% factor(., levels = unique(VISIT)))

  hue <- 0.55
  visit_category_colors <-
    c("red","goldenrod1",
      hsv(h = hue, s = 0.2, v = 0.95),
      hsv(h = hue, s = 0.35, v = 0.95),
      hsv(h = hue, s = 0.5, v = 0.95),
      "gray80","gray60", # phone visits
      hsv(h = hue, s = 1, v = 0.95)
    )


  visit_type_values <- c("Planned","Suppl.")

  factor_values <-
    bind_rows(
      tibble(
        var = "AVISIT", values = tmp$AVISIT,
        colors = visit_category_colors[tmp$visit_category+1]
      ),
      tibble(
        var = "VISIT", values = tmp$VISIT %>% unique(),
        colors = visit_category_colors
      ),
      tibble(
        var = "VISIT_TYPE", values = visit_type_values,
        colors = "black"
      )
    )

  variable_info <-
    bind_rows(
      tibble(var = "AVISITN", label = "Visit number", type = "double"),
      tibble(var = "AVISIT", label = "Visit", type = "factor"),
      # tibble(var = "ADT", label = "Visit date", type = "Date"),
      tibble(var = "DAY", label = "Study day", type = "integer"),
      tibble(var = "VISIT", label = "Visit", type = "factor"),
      tibble(var = "VISIT_TYPE", label = "Visit type", type = "factor"),
      tibble(var = "PIPV", label = "Is a planned in-person visit", type = "logical"),
      tibble(var = "OPIPV", label = "Is an 'outcome' planned in-person visit", type = "logical")
    )

  visits <-
    ADSX %>%
    filter(USUBJID %in% subjects$USUBJID) %>%
    select(USUBJID, AVISITN, AVISIT, ADY) %>%  # ADT
    rename(DAY = ADY) %>%
    distinct() %>%
    mutate(
      # ADT = ADT %>% as.Date(., format = date_format),
      DAY = DAY %>% as.integer(),
      AVISIT = AVISIT %>% factor(., levels = tmp$VISIT %>% unique()),
      visit_category = floor(AVISITN)
    ) %>%
    left_join(
      ., tmp %>% select(visit_category, VISIT) %>% distinct(), by = "visit_category"
    ) %>%
    select(-visit_category) %>%
    mutate(
      VISIT_TYPE =
        ifelse(AVISITN == floor(AVISITN), "Planned","Suppl.") %>%
        factor(., levels = visit_type_values),
      PIPV = (AVISITN %in% c(0:4,7)),
      OPIPV = (AVISITN %in% c(2:4,7))
    )

  list(
    visits = visits,
    variable_info = variable_info,
    factor_values = factor_values
  )

}
