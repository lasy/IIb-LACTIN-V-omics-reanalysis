

get_mtb_loadings_for_plot <- function(res, xaxis, yaxis, VIP = TRUE, boot){
  
  faX <- get_mtb_loadings(res, cut_at_nf = FALSE)
  
  if (VIP)
    faX <-
      faX |>
      left_join(
        get_mtb_vipc(res = res, boot = boot) |>
          dplyr::rename(vipc = value, vipc_lo = lo, vipc_up = up, vipc_mean = mean, vipc_sd = sd),
        by = join_by(variable, block)
      ) |>
      left_join(get_ref_vipc(res), by = join_by(variable, block)) |>
      mutate(
        rel_imp = vipc / vipc_ref,
        median_rel_imp = ifelse(is.na(median), rel_imp, median / vipc_ref)
      ) |>
      filter((rel_imp > 1) | (median_rel_imp > 1), (rel_imp > 0.8) & (median_rel_imp > 0.8))
  
  
  faX$xaxis <- faX[, str_c('Ax',xaxis)] %>% unlist()
  faX$yaxis <- faX[, str_c('Ax',yaxis)] %>% unlist()
  faX
}

get_mtb_lX_for_plot <- function(res, xaxis, yaxis, samples_color = NULL){
  Xrow <- res$lX |> as_tibble() |>  mutate(sampleID = rownames(res$lX))
  Xrow <- Xrow |> .add_colors(samples_color)
  Xrow$xaxis <- Xrow[, str_c("Ax", xaxis)] %>% unlist()
  Xrow$yaxis <- Xrow[, str_c("Ax", yaxis)] %>% unlist()
  Xrow
}

plot_mtb_biplot <- function(res, xaxis, yaxis, scale_axes = "eig", 
                            samples_color, sample_color_scale = list(name = "", values = c(1:3)), 
                            block_color_scale = list(name = "", values = block_colors, breaks = block_names), 
                            loading_scaling = 20,
                            VIP = TRUE, boot){
  
  res_loadings <- get_mtb_loadings_for_plot(res = res, xaxis = xaxis, yaxis = yaxis, VIP = VIP, boot = boot) |> 
    mutate(
      xaxis = xaxis * loading_scaling,
      yaxis = yaxis * loading_scaling
    )
  
  res_scores <- get_mtb_lX_for_plot(res, xaxis = xaxis, yaxis = yaxis, samples_color = samples_color)
  
  
  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }
  
  inertia_values <- get_mtb_eig(res)
  
  xaxis_label <- str_c('Axis ',xaxis, " (", (100 * inertia_values$var[xaxis]) |> round(), "% of total inertia)")
  yaxis_label <- str_c('Axis ',yaxis, " (", (100 * inertia_values$var[yaxis]) |> round(), "% of total inertia)")
  
  
  ggplot(res_scores, aes(x = xaxis, y = yaxis)) +
    coord_fixed(ratio = asp) +
    geom_vline(xintercept = 0, col = "gray50") +
    geom_hline(yintercept = 0, col = "gray50") +
    xlab(xaxis_label) +
    ylab(yaxis_label) +
    
    # scores
    geom_point(aes(col = samples_color), alpha = 0.6) +
    stat_ellipse(aes(col = samples_color), alpha = 0.5) +
    scale_color_manual(
      sample_color_scale$name, values = sample_color_scale$values,
      guide = guide_legend(override.aes = list(size = 3, alpha = 1)) # override aesthetics for legend
      ) +
    ggnewscale::new_scale_color() +
    
    # loadings
    geom_segment(
      data = res_loadings,
      aes(xend = 0, yend = 0, col = block),
      arrow = arrow(ends = "first", type = "closed", length = unit(10,"pt"))
    ) +
    geom_label_repel(
      data = res_loadings,
      aes(label = variable, col = block), 
      min.segment.length = 1, size = 3
    )  +
    scale_color_manual(block_color_scale$name, values = block_color_scale$values, breaks = block_color_scale$breaks) +
    guides(col = "none")
}
