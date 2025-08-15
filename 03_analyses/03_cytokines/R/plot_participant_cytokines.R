
plot_participants_cytokines <- function(df){
  
  df <-
    df %>%
    mutate(
      visit_label = get_visit_labels(AVISITN) |>  fct_drop(),
      x = visit_label |> as.numeric()
    )
  
  visits <- 
    df %>% 
    select(AVISITN, visit_label, x) |> 
    distinct() |> 
    arrange(x)

  x_breaks <- visits$x
  x_labels <- visits$visit_label

  ggplot(df, aes(x = x, y = trans_log10_conc, col = cytokine)) +
    geom_line() +
    geom_point() +
    facet_wrap(USUBJID ~ .) +
    scale_x_continuous("Visit", breaks = x_breaks, labels = x_labels) +
    ylab("transformed log10(concentrations)")
  
 
  
}
