
plot_n_swabs_per_visit <- function(visits, subjects, by = "ARM") {

  df <-
    visits %>%
    filter(!is.na(N_SWABS), PIPV) %>%
    left_join(subjects %>% select(USUBJID, SITENAME, ARM), by = "USUBJID")

  df$by <- df[, by] %>% unlist()
  
  df <- 
    df |> 
    group_by(USUBJID) |> 
    mutate(tot_n_swabs = sum(N_SWABS)) |> 
    ungroup() |> 
    arrange(-tot_n_swabs) |> 
    mutate(USUBJID = USUBJID |> factor(levels = unique(USUBJID)))

  ggplot(df,
         aes(x = AVISIT, y = USUBJID, col = N_SWABS %>% as.factor())) +
    geom_point() +
    scale_y_discrete(breaks = NULL) +
    xlab("") +
    ylab("Participants") +
    scale_color_manual("Number of shipped swabs\nat planned\nin-person visits", values = c("red2", "steelblue2", "steelblue4")) +
    facet_grid(by ~ ., scales = "free", space = "free") +
    theme(
      axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
  

}
