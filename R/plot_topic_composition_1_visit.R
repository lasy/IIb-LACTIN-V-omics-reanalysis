
plot_topic_composition_1_visit <- function(gammas, visit = 0){

  gammas_visit <-
    gammas %>%
    filter(AVISITN == visit, EOSSTT == "COMPLETED")

  gammas_visit <-
    gammas_visit %>%
    arrange_participants_by_topic(., visit = visit)
  
  gammas_visit |> 
    ggplot() +
    aes(x = Barcode, y = prop, fill = topic_label) +
    geom_col() +
    scale_x_discrete("Samples", breaks = NULL) +
    ylab("topic proportion") +
    facet_grid(. ~ ARM, scales = "free", space = "free") +
    scale_fill_manual(
      "Topic",
      breaks = gammas_visit$topic_label %>% levels(),
      values = gammas_visit$topic_label %>% levels() %>% get_topic_colors()
      )
}
