
get_sex_visit_level <- function(events, visits) {


  tmp <-
    bind_rows(
      visits %>%
        filter(AVISITN > 1) %>%
        select(USUBJID, AVISITN, DAY) %>%
        mutate(CATEGORY = "Visit", variable = "Visit") ,
      events %>%
        filter(CATEGORY == "Sexual behavior") %>%
        select(USUBJID, DAY, CATEGORY, VARIABLE, NUMBER)
    ) %>%
    mutate(CATEGORY = CATEGORY %>% factor(., levels = c("Visit", "Sexual behavior"))) %>%
    arrange(USUBJID, DAY, CATEGORY) %>%
    group_by(USUBJID) %>%
    mutate(visit_id = cumsum(!is.na(AVISITN)) - !is.na(AVISITN))

  summary_statistics <-
    tmp %>%
    filter(CATEGORY != "Visit") %>%
    group_by(USUBJID, visit_id) %>%
    summarize(
      N_SEX= sum(NUMBER, na.rm = TRUE),
      N_PROT_SEX = sum(NUMBER[VARIABLE == "Sex with condoms"], na.rm = TRUE),
      N_UNPROT_SEX = N_SEX - N_PROT_SEX,
      last_sex_day = max(DAY),
      .groups = "drop"
    )

  summary_statistics_protected <-
    tmp %>%
    filter(CATEGORY != "Visit", VARIABLE == "Sex without condoms") %>%
    group_by(USUBJID, visit_id) %>%
    summarize(
      last_unprot_sex_day = max(DAY),
      .groups = "drop"
    )

  summary_statistics_unprotected <-
    tmp %>%
    filter(CATEGORY != "Visit", VARIABLE != "Sex without condoms") %>%
    group_by(USUBJID, visit_id) %>%
    summarize(
      last_prot_sex_day = max(DAY),
      .groups = "drop"
    )

  tmp_visit_level <-
    tmp %>%
    filter(CATEGORY == "Visit") %>%
    left_join(summary_statistics, by = c("USUBJID", "visit_id")) %>%
    left_join(summary_statistics_protected, by = c("USUBJID", "visit_id")) %>%
    left_join(summary_statistics_unprotected, by = c("USUBJID", "visit_id")) %>%
    arrange(USUBJID, AVISITN) %>%
    mutate(
      LAST_VISIT = DAY - lag(DAY),
      LAST_SEX = DAY - last_sex_day,
      LAST_UNPROT_SEX = DAY - last_unprot_sex_day,
      LAST_PROT_SEX = DAY - last_prot_sex_day,
      SEX_FREQ = N_SEX/LAST_VISIT,
      PROT_SEX_FREQ = N_PROT_SEX/LAST_VISIT,
      UNPROT_SEX_FREQ = N_UNPROT_SEX/LAST_VISIT,
    ) %>%
    select(
      USUBJID, AVISITN,
      N_SEX, N_PROT_SEX, N_UNPROT_SEX,
      LAST_SEX, LAST_PROT_SEX, LAST_UNPROT_SEX,
      SEX_FREQ, PROT_SEX_FREQ, UNPROT_SEX_FREQ
    )


  variable_info <-
    bind_rows(
      tibble(
        var_name = 'N_SEX',
        var_name_print = "Total number of sexual intercourses since last visit"
      ),
      tibble(
        var_name = 'N_PROT_SEX',
        var_name_print = "Number of protected (condoms) sexual intercourses since last visit"
      ),
      tibble(
        var_name = 'N_UNPROT_SEX',
        var_name_print = "Number of unprotected (no condom) sexual intercourses since last visit"
      ),
      tibble(
        var_name = 'LAST_SEX',
        var_name_print = "Number of days since last sexual intercourse"
      ),
      tibble(
        var_name = 'LAST_PROT_SEX',
        var_name_print = "Number of days since last protected (condoms) sexual intercourse"
      ),
      tibble(var_name = 'LAST_UNPROT_SEX',
             var_name_print = "Number of days since last unprotected (no condom) sexual intercourse"
      )
    ) %>% mutate(var_type = "integer") %>%
    bind_rows(
      bind_rows(
        tibble(
          var_name = 'SEX_FREQ',
          var_name_print = "Average sex frequency since last visit"
        ),
        tibble(var_name = 'PROT_SEX_FREQ',
               var_name_print = "Average protected (condoms) sex frequency since last visit"
        ),
        tibble(var_name = 'UNPROT_SEX_FREQ',
               var_name_print = "Average unprotected (no condoms) sex frequency since last visit"
        )
      ) %>% mutate(var_type = "double")
    ) %>%
    mutate(
      var_group = "Sexual behavior"
    )


  list(
    sex_events_summary = tmp_visit_level,
    variable_info = variable_info
  )
}
