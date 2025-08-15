
compare_sex_summaries <- function(sex_events_summary, sex_summary_AD) {

  c1 <-
    sex_summary_AD %>%
    filter(PARAM == "How many times did you have sex since the last visit?") %>%
    mutate(AVALC = AVALC %>% as.integer()) %>%
    left_join(sex_events_summary, by = c("USUBJID", "AVISITN"))

  g1 <-
    ggplot(c1, aes(x = AVALC, N_SEX)) +
    geom_point(alpha = 0.1) +
    xlab("How many times did you have sex since the last visit? (visit survey)") +
    ylab("Total number of sexual intercourse since last visit reported in daily surveys") +
    facet_wrap(AVISITN ~ ., labeller = label_both)

  c2 <-
    sex_summary_AD %>%
    filter(PARAM == "If you had sex with a man, did your partner use a condom each time?") %>%
    left_join(sex_events_summary, by = c("USUBJID", "AVISITN")) %>%
    mutate(condom_each_time = (N_SEX == N_PROT_SEX)) %>%
    filter(!is.na(condom_each_time), AVALC != "Not applicable")

  g2 <-
    ggplot(c2, aes(x = AVALC, condom_each_time)) +
    geom_jitter(alpha = 0.3, height = 0.25, width = 0.25) +
    xlab("If you had sex with a man, did your partner use a condom each time?") +
    ylab("Total sex == total sex with condoms") +
    facet_wrap(AVISITN ~ ., labeller = label_both)

  c3 <-
    sex_summary_AD %>%
    filter(PARAM == "How many days has it been since the last time you had sex?") %>%
    mutate(AVALC = AVALC %>% as.integer()) %>%
    left_join(sex_events_summary, by = c("USUBJID", "AVISITN"))

  g3 <-
    ggplot(c3, aes(x = AVALC, LAST_SEX)) +
    geom_point(alpha = 0.3) +
    xlab("How many days has it been since the last time you had sex?") +
    ylab("days since last sex") +
    facet_wrap(AVISITN ~ .)

  list(g1, g2, g3)
}
