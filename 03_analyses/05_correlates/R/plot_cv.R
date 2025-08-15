
plot_cv <- function(res, selected_metric = "all"){
  
  possible_metrics <- c("all", unique(res$cv$summary$metric))
  if (!(selected_metric %in% possible_metrics))
    stop(str_c("Invalid metric. `metric` should be one of", str_c(possible_metrics, collapse = ", ")))
  
  ylab <- "Mean "
  
  df <- res$cv$summary
  if (selected_metric != "all"){
    df <- df |> filter(metric == selected_metric) 
    ylab <- str_c(ylab, selected_metric)
  } else {
    ylab <- str_c(ylab, "value")
  }
  ylab <- ylab |> str_c(" (& 95% CI)")
  
  optdim <- res$opt_ncomp
  
  cv <- df |> filter(set != "random predictions")
  rand_perm <- df |> filter(set == "random predictions") 
  
  gg <- 
    df  |> 
    ggplot(aes(x = dim, y = mean, color = set)) +
    geom_vline(xintercept = optdim, color = "turquoise3", linewidth = 6, alpha = 0.25) +
    geom_hline(data = rand_perm, aes(yintercept = mean), color = "gray60") +
    geom_rect(data = rand_perm, aes(xmin = -Inf, xmax = Inf, ymin = `95%CI_lo`, ymax = `95%CI_hi`), fill = "gray60", alpha = 0.25, col = NA) +
    geom_label(data = rand_perm, aes(x = Inf, y = mean), label = "random perm.", hjust = 1, vjust = 0.5, size = 3, color = "gray60") +
    geom_linerange(aes(ymin = `95%CI_lo`, ymax = `95%CI_hi`), alpha = 0.5, linewidth = 2, lineend = "round") +
    geom_point(size = 2) +
    geom_line() +
    scale_x_continuous("Number of components", breaks = 1:max(df$dim, na.rm = TRUE), minor_breaks = NULL) +
    ylab(ylab) +
    scale_color_manual("", values = c("calibration" = "black", "validation" = "turquoise3", "random predictions" = "gray60")) 
  
  if (selected_metric == "all") gg <- gg + facet_wrap(metric ~ ., scales = "free_y", nrow = 1)
  gg
  
}
