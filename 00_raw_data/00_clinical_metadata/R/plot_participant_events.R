
plot_participant_events <- function(p_events, title = FALSE){

  p_events <-
    p_events %>%
    mutate(
      y =
        case_when(
          (CATEGORY != "Symptoms") & (!is.na(VALUE)) ~ str_c(VARIABLE, " (",VALUE,")"),
          TRUE ~ VARIABLE
        ),
      fill =
        case_when(
          (CATEGORY != "Symptoms") ~ NA_character_,
          TRUE ~ VALUE
        )
    ) %>%
    filter(! ((CATEGORY == "Symptoms") & (VALUE == "None")))

  g <-
    ggplot(p_events %>%
           filter(y != "Dose # since study start"),
         aes(x = DAY/7, y = y, fill = fill)) +
    geom_vline(xintercept = 0, col = "gray70", size = 2) +
    geom_tile() + # fill = "gray50"
    scale_fill_manual("Symptom severity", breaks = c("Mild","Moderate","Severe"),
                      values = c("gold","darkorange","firebrick")) +
    facet_grid(CATEGORY ~ ., scales = "free", space = "free") +
    ylab("") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0),
          legend.position = "bottom") +
    scale_x_continuous("weeks since study start", breaks = seq(-4, 24, by = 4), minor_breaks = -4:24)

  if (title) {
    g <- g + ggtitle(unique(p_events$USUBJID))
  }

  g

}
