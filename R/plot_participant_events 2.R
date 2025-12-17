
plot_participant_events <- function(p_events, title = FALSE, p_visits = NULL, x_scale = TRUE){
  
  if (length(unique(p_events$USUBJID)) != 1) 
    stop("`p_events` should have data for only one participant.\n")

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
           aes(x = DAY/7, y = y, fill = fill))
  
  if (!is.null(p_visits)) {
    if (length(unique(p_visits$USUBJID)) != 1) 
      stop("`p_visits` should have data for only one participant.\n")
    p_visits <- p_visits %>% mutate(VISIT_CAT = ifelse(round(AVISITN) %in% 5:6, "phone","in-person"))
    g <- g + geom_vline(data = p_visits, aes(xintercept = DAY/7, linetype = VISIT_CAT), col = "slategray2") +
      scale_linetype("Visit type")
  } else {
    g <- g + geom_vline(xintercept = 0, col = "gray70", linewidth = 2) 
  }

  g <- g +
    geom_tile() + # fill = "gray50"
    scale_fill_manual("Symptom severity", breaks = c("Mild","Moderate","Severe"),
                      values = c("gold","darkorange","firebrick")) +
    facet_grid(CATEGORY ~ ., scales = "free", space = "free") +
    ylab("") +
    theme(strip.text.y = element_text(color = "black", angle = 0, hjust = 0),
          legend.position = "bottom") 
  
  if (x_scale) {
    g <- g  +
      scale_x_continuous("weeks since study start", breaks = seq(-4, 24, by = 4), minor_breaks = -4:24)
  }
 
  if (title) {
    g <- g + ggtitle(unique(p_events$USUBJID))
  }

  g

}
