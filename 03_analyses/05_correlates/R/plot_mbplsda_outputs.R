
plot_mbplsda_outputs <- function(mod_res){
  
  block_colors <- get_block_colors(mod_res$inputs |> names())
  
  g_blocks <- 
    plot_mtb_blocks(Xs = mod_res$inputs, Y = list("Endpoint" = mod_res$output), block_colors = block_colors) & 
    theme(strip.text.x = element_text(angle = 90, hjust = 0))
  
  g_cv <- plot_cv(mod_res, selected_metric = "all")
  
  g_eig <- plot_mtb_eig(mod_res$res, xaxis = 1, yaxis = 2)
  
  g_conf_mat <- 
    plot_confusion_matrix(
      true_outcomes = mod_res$output_fct, 
      pred_outcomes = mod_res$predictions$pred_fct) + 
    labs(subtitle = str_c("Accuracy: ", mean(mod_res$predictions$pred_fct == mod_res$output_fct) |> round(2)))
  
  g_cov <- 
    plot_mtb_cov(mod_res$res, xaxis = 1, yaxis = 2) +
    scale_color_manual(values = block_colors)
  
  g_scores <- 
    plot_mtb_lX(mod_res$res, xaxis = 1, yaxis = 2, samples_color = mod_res$output_fct, scale_axes = "eig") +
    scale_color_manual("", values = c(get_topic_colors(c("I","III")), "dodgerblue1")) +
    stat_ellipse(aes(col = mod_res$output_fct))
  
  g_loadings <- 
    plot_mtb_Xloadings(res = mod_res$res) +
    scale_color_manual(values = block_colors)
  
  g_loadings_VIP <- 
    plot_mtb_Xloadings(res = mod_res$res, boot = mod_res$boot, VIP = TRUE) +
    scale_color_manual(breaks = names(mod_res$inputs), values = block_colors)
  
  g_biplot <- 
    plot_mtb_biplot(
      res = mod_res$res, xaxis = 1, yaxis = 2,  scale_axes = "eig",
      samples_color = mod_res$output_fct, 
      sample_color_scale = list(name = "", values = get_topic_colors(c("I","III", "IV"))),
      block_color_scale = list(name = "", values = block_colors),
      VIP = TRUE, boot = mod_res$boot
    )
  
  g_bipc <- 
    plot_mtb_bipc(res = mod_res$res, boot = mod_res$boot) +
    scale_color_manual(values = block_colors)
  
  g_bipc2 <- 
    plot_mtb_bipc(res = mod_res$res, boot = mod_res$boot, show_ref_ratio = TRUE) +
    scale_color_manual(values = block_colors)
  
  g_relbipc <-
    plot_mtb_relbipc(res = mod_res$res, boot = mod_res$boot) +
    scale_color_manual(values = block_colors)
  
  g_blocks + # A B
    g_cv + # C
    g_eig[[1]] + g_eig[[2]] + g_conf_mat + g_cov + #  D E F G
     g_scores + g_loadings + g_biplot + # H I J
    g_bipc + g_bipc2 + g_relbipc + # K L M
    plot_layout(
      guides = "collect",
      design = "
      AAAAAB
      CCCCCC
      DEFFGG
      HHIIJJ
      KKLLMM
      "
      ) 
}