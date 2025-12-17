
plot_pca_correlation_circle <- function(data, pca_res, pca_var, PCx = 1, PCy = 2) {
  
  # first, we compute the correlations between the data in the original axes and the data in the PC system
  
  correlations <- cor(data, pca_res$x) %>% as.data.frame()
  correlations$pcx <- correlations[,PCx] 
  correlations$pcy <- correlations[,PCy] 
  correlations <- 
    correlations %>% 
    mutate(column = rownames(correlations))
  
  x_lab <- str_c("PC", PCx, " (",round(100 * pca_var$percent[PCx]),"%)")
  y_lab <- str_c("PC", PCy, " (",round(100 * pca_var$percent[PCy]),"%)")
  
  arrow_style <- 
    arrow(
      angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
    )
  
  ggplot(correlations %>% as.data.frame(), 
         aes(x = pcx, y = pcy, col = column)) +
    geom_vline(xintercept = 0, col = "gray", linetype = 2) +
    geom_hline(yintercept = 0, col = "gray", linetype = 2) +
    annotate(
      "path",
      x = cos(seq(0, 2*pi,length.out=100)),
      y = sin(seq(0, 2*pi,length.out=100)),
      col = "gray"
    ) +
    ggrepel::geom_label_repel(
      aes(x = pcx, # + 0.05 * pcx / sqrt(pcx^2 + pcy^2), 
          y = pcy, # +  0.05 * pcy / sqrt(pcx^2 + pcy^2), 
          label = column),
      segment.colour = NA
    ) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow_style) +
    xlab(x_lab) +
    ylab(y_lab) +
    guides(col = "none") + 
    coord_fixed()
  
}
