get_cv_error_rates <- function(res_testdim){
  bind_rows(
    res_testdim$ErrorRateCglobal.max |> as_tibble() |> filter(variable == "global") |> mutate(set = "calibration"),
    res_testdim$ErrorRateVglobal.max |> as_tibble() |> filter(variable == "global") |> mutate(set = "validation")
  ) |>
    mutate(n = dimlab |> parse_number()) 
}

plot_cv_error_rates <- function(res){
  if (is.null(res$cv_error_rates)) stop("No cross-validation error rates provided.")
  if (is.null(res$opt_ncomp)) {
    warning("No optimal number of components provided. Choosing the component with the smallest validation error rate.")
    res$opt_ncomp <- cv_error_rates |> filter(set == "validation") |> filter(mean == min(mean)) |> pull(n)
  }
  
  cv_error_rates <- res$cv_error_rates
  optdim <- res$opt_ncomp
  
  plot <- 
    cv_error_rates |> 
    ggplot(aes(x = n, y = mean, color = set)) +
    geom_vline(xintercept = optdim, color = "gray80", linewidth = 6, alpha = 0.5) +
    geom_linerange(aes(ymin = `95CIinf`, ymax = `95CIsup`), alpha = 0.5, linewidth = 2, lineend = "round") +
    geom_point(size = 2) +
    geom_line() +
    scale_x_continuous("Number of components", breaks = 1:max(cv_error_rates$n), minor_breaks = NULL) +
    scale_y_continuous("Mean error rate (& 95% CI)") +
    scale_color_manual("", values = c("calibration" = "gray40", "validation" = "turquoise3")) 
  
  if (!is.null(res$model_name)) 
    plot <- plot + ggtitle(res$model_name |> str_wrap(25))
  
  plot
}
