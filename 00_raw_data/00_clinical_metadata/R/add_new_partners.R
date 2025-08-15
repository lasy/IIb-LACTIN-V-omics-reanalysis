
add_new_partners <- function(sex_visit_level, sex_summary_AD){

  sex_visit_level$sex_events_summary <-
    sex_visit_level$sex_events_summary  %>%
    left_join(
      sex_summary_AD %>%
        filter(PARAM == "How many new partners did you have?")  %>%
        mutate(N_NEW_PARTNERS = AVALC %>% as.integer()) %>%
        select(USUBJID, AVISITN, N_NEW_PARTNERS),
      by = c("USUBJID", "AVISITN")
    )

  sex_visit_level$variable_info <-
    sex_visit_level$variable_info %>%
    bind_rows(
      .,
      tibble(
        var_name = "N_NEW_PARTNERS", var_name_print = "Number of new sexual partners since last visit",
        var_type = "integer", var_group = "Sexual behavior"
      )
    )


  sex_visit_level
}
