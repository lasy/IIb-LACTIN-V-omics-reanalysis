
plot_visit_dates <- function(visits, subjects) {
  ggplot(visits %>%
           left_join(., subjects, by = "USUBJID"),
         aes(x = DAY, y = USUBJID, col = VISIT)) +
    geom_point(aes(shape = VISIT_TYPE)) +
    scale_color_manual(
      breaks = get_fct_values("VISIT"), values = get_fct_colors("VISIT")
    ) +
    scale_shape(get_print_name("VISIT_TYPE")) +
    ylab("Participants") +
    xlab(get_print_name("DAY")) +
    scale_y_discrete(breaks = NULL) +
    facet_grid(EOSSTT ~ ., scales = "free", space = "free") +
    scale_x_continuous(
      breaks = seq(-70, 500, by = 28), minor_breaks = seq(-70, 500, by = 7)
    )
}
