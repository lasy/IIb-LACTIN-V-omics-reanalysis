
plot_models_cv <- function(full, ext, red, min, self, opt_ncomp, selected_metric = "average F1 score") {
  tmp <- 
    bind_rows(
      full$cv$summary |> filter((dim == opt_ncomp) | is.na(dim), metric == selected_metric) |> 
        mutate(Model = "Full"),
      ext$cv$summary |> filter((dim == opt_ncomp) | is.na(dim), metric == selected_metric) |> 
        mutate(Model = "Extended"),
      red$cv$summary |> filter((dim == opt_ncomp) | is.na(dim), metric == selected_metric) |> 
        mutate(Model = "Reduced"),
      min$cv$summary |> filter((dim == opt_ncomp) | is.na(dim), metric == selected_metric) |> 
        mutate(Model = "Minimum"),
      self$cv$summary |> filter((dim == opt_ncomp) | is.na(dim), metric == selected_metric, set != 
                                  "whole dataset") |>
        mutate(Model = "Self-predicting")
    ) |> 
    mutate(Model = Model |> fct_inorder() |> fct_rev()) 
  
  ylab <- str_c(selected_metric, "\n(mean & 95%CI)")
  if (selected_metric == "average F1 score") ylab <- str_c("Ave. F1 (mean & 95%CI)")
  
  tmp |> 
    ggplot(aes(x = Model, y = mean, col = set)) +
    geom_errorbar(aes(ymin = `95%CI_lo`, ymax = `95%CI_hi`), width = 0.2) +
    # ggplot(aes(x = Model, y = median, col = set)) +
    # geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.1) +
    geom_point() +
    geom_text(data = tmp |> filter(Model == "Full"), aes(label = set |> str_sub(1,1)), hjust = 0, vjust = 0, nudge_x = 0.2) +
    scale_color_manual("", values = c("calibration" = "black", "validation" = "turquoise3", "random permutation" = "gray")) +
    ylab(ylab) + xlab("") +
    guides(col = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
