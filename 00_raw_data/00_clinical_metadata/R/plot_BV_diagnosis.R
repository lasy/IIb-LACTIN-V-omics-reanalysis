
plot_BV_diagnosis <- function(visits, subjects, show = c("BV","NUGENT"), sort_by = c("n_BV", "tot_nugent")) {

  show <-  show[1]
  show <- match.arg(show, c("BV","NUGENT"))

  sort_by <-  sort_by[1]
  sort_by <- match.arg(sort_by, c("n_BV", "tot_nugent"))

  df <-
    visits %>%
    left_join(subjects, by = "USUBJID") %>%
    group_by(USUBJID) %>%
    mutate(
      n_BV = sum(BV == "Yes", na.rm = TRUE),
      tot_nugent = sum(NUGENT %>% as.integer(), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(across(all_of(sort_by))) %>%
    mutate(USUBJID = USUBJID %>% factor(., levels = unique(USUBJID)))

  df$color <- df[,show] %>% unlist() %>%   as.character()


  ggplot(df, aes(x = DAY, y = USUBJID, col = color)) +
    geom_point() +
    scale_color_manual(
      get_print_name(show),
      breaks = get_fct_values(show),
      values = get_fct_colors(show),
      na.value = "gray80"
    ) +
    facet_grid(ARM ~ ., scales = "free", space = "free") +
    xlab(get_print_name("DAY")) +
    scale_y_discrete("Participants", breaks = NULL) +
    scale_x_continuous(
      breaks = seq(-70, 500, by = 28),
      minor_breaks = seq(-70, 500, by = 7)
    )
}


