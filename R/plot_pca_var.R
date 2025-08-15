
plot_pca_var <- function(pca_var) {
  
  g_var <- 
    ggplot(pca_var, aes(PC, percent)) +
    geom_col(fill = "steelblue1", alpha = 0.8) +
    scale_x_continuous(breaks = unique(pca_var$PC), minor_breaks = NULL) +
    scale_y_continuous(
      "% of variance",
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.01))
    )
  
  g_cumvar <- 
    ggplot(pca_var, aes(PC, cumulative)) +
    geom_col(fill = "steelblue1", alpha = 0.8) +
    scale_x_continuous(breaks = unique(pca_var$PC), minor_breaks = NULL) +
    scale_y_continuous(
      "Cummulative % of variance",
      breaks = seq(0,1, by = 0.2),
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.01))
    )
  
  g_var + g_cumvar
}
