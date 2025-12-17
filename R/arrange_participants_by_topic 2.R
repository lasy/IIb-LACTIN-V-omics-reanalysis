
arrange_participants_by_topic <- function(gammas, visit = 0) {
  gammas_visit <- gammas %>% filter(AVISITN == visit)
  gammas_visit <-
    gammas_visit %>%
    arrange(USUBJID, -prop) %>%
    group_by(USUBJID) %>%
    mutate(
      main_topic = topic[1],
      mean_topic = weighted.mean(topic %>% as.numeric(), prop)
    ) %>%
    ungroup() %>%
    arrange(main_topic, mean_topic) %>%
    mutate(
      USUBJID = USUBJID %>% factor(., levels = unique(USUBJID))
    )

  gammas %>%
    mutate(
      USUBJID = USUBJID %>% factor(., levels = levels(gammas_visit$USUBJID))
      ) %>%
    arrange(USUBJID, AVISITN) %>%
    mutate(
      Barcode = Barcode %>% factor(., levels = unique(Barcode))
    )
}


arrange_samples_by_topic <- function(gammas) {

  gammas %>%
    arrange(Barcode, -prop) %>%
    group_by(Barcode) %>%
    mutate(
      main_topic = topic[1],
      mean_topic = weighted.mean(topic %>% as.numeric(), prop)
    ) %>%
    ungroup() %>%
    arrange(main_topic, mean_topic) %>%
    mutate(
      Barcode = Barcode %>% factor(., levels = unique(Barcode))
    )
}
