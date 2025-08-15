
combine_mtb_bipcs <- function(list_of_mbpls_res) {
  list_of_mbpls_res %>% 
    map(
      ~ get_mtb_bipc(.x$res, boot = .x$boot) |>  
        mutate(model = .x$model_name)
    ) |> 
    bind_rows() |> 
    mutate(model = model |> factor(levels = unique(model)))
}


plot_mtb_combined_bipcs <- function(bipcs){
  ggplot(bipcs, aes(y = block %>% fct_rev(), col = block)) +
    geom_segment(
      aes(yend = block %>% fct_rev(), x = lo, xend = up), 
      linewidth = 0.75, alpha = 0.4, lineend = "round"
    ) +
    geom_point(aes(x = value), size = 1.5) +
    facet_grid(. ~ model) +
    xlab("Relative block importance") +
    ylab("Explanatory blocks") +
    guides(col = "none") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) 
}

combine_mtb_vipcs <- function(list_of_mbpls_res) {
  list_of_mbpls_res %>% 
    map(
      ~ get_mtb_vipc(.x$res, boot = .x$boot) |>  
        mutate(model = .x$model_name)
    ) |> 
    bind_rows() |> 
    mutate(model = model |> factor(levels = unique(model)))
}


combine_mtb_coefs <- function(list_of_mbpls_res) {
  list_of_mbpls_res %>% 
    map(
      ~ get_mtb_coef(.x$res, boot = .x$boot) |>  
        mutate(model = .x$model_name)
    ) |> 
    bind_rows() |> 
    mutate(model = model |> factor(levels = unique(model)))
}
