
plot_nugent_sub_scores <- function(visits) {

  ggplot(
    visits %>%
      filter(
        !is.na(BV),
        !is.na(AMSEL)
      ) %>%
      select(USUBJID, AVISITN, BV, AMSEL, starts_with("NUGENT_")) %>%
      pivot_longer(
        cols =  starts_with("NUGENT_"),
        names_to = "score_type",
        values_to = "score"
      ) %>%
      mutate(
        score = score %>% factor(., levels = c(0:10)),
        score_type = score_type %>% factor(),
        Diagnosis =
          ifelse(BV == "Yes", "BV","Not BV") %>%
          factor(., levels = c("Not BV","BV"))
      ) %>%
      filter(!is.na(score)),
    aes(x = AMSEL, y = score, col = Diagnosis)
  ) +
    geom_jitter(height = 0.1, width = 0.1, alpha = 0.3, size = 0.5) +
    facet_grid(score_type ~ ., scales = "free_y", space = "free_y") +
    scale_color_manual(values = get_fct_colors("BV")[2:1]) +
    # guides(col = "none") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))

}
