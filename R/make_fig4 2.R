
make_fig4 <- function(res, boot, varY){
  g_a <- 
    plot_mtb_blocks(
      Xs = res$tabX %>% as_blocks(., p = res$blo), 
      Y = list("Microbiota\n(16S topics)\nat target visit" = res$tabY)
    )
  
  g_b <- plot_mtb_cov(res = res, scale_axes = "eig")
  
  g_c <- 
    plot_mtb_bipc(res = res, boot = boot) # + coord_flip() + 
  # theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  g_d <- plot_mtb_coef(res = res, boot = boot, Y_var = varY, format = "long")
  
  # design <- "
  # AA
  # BD
  # CD
  # "
  # 
  # (g_a) - g_b + g_c + g_d +
  #   plot_layout(design = design) +
  #   plot_annotation(tag_level = c("a", "1"))
  
  
  fig4 <- 
    ggpubr::ggarrange(
      g_a,
      ggpubr::ggarrange(
        ggpubr::ggarrange(g_b, g_c, nrow = 2, ncol = 1, labels = c("b","c")),
        g_d, nrow = 1, ncol = 2, labels = c("", "d"), widths = c(1, 1.5)
      ),
      nrow = 2, ncol = 1, heights = c(1, 2), labels = c("a", "")
    )
  
  fig4
  
}