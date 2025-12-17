
plot_pca_samples <- function(Sample_PCs, pca_var, PCx = 1, PCy = 2, color_by = NULL, annot = NULL){
  
  Sample_PCs$pcx <- Sample_PCs[,str_c(".fittedPC",PCx)] %>% unlist()
  Sample_PCs$pcy <- Sample_PCs[,str_c(".fittedPC",PCy)] %>% unlist()
  
  if (!is.null(color_by)) {
    Sample_PCs$color <- Sample_PCs[, color_by] %>% unlist()
  } else {
    Sample_PCs$color <- 0
  }
  
  g <- 
    ggplot(Sample_PCs, aes(x = pcx, y = pcy, col = color)) + 
    # coord_fixed(pca_var$std.dev[PCy]/pca_var$std.dev[PCx]) +
    coord_fixed() +
    geom_vline(xintercept = 0, col = "gray") +
    geom_hline(yintercept = 0, col = "gray") +
    geom_point(size = 1.5) +
    xlab(str_c("PC", PCx," (",round(100 * pca_var$percent[PCx]),"%)")) +
    ylab(str_c("PC", PCy," (",round(100 * pca_var$percent[PCy]),"%)"))
  
  if (is.null(color_by)) {
    g <- g + guides(col = "none") + scale_color_gradient(low = "black", high = "black")
  }
  
  if (!is.null(annot)) {
    annot$pcx <- annot[,str_c(".fittedPC",PCx)] %>% unlist()
    annot$pcy <- annot[,str_c(".fittedPC",PCy)] %>% unlist()
    g <- 
      g +
      geom_point(data = annot, col = "black", shape = 1, size = 2) +
      geom_text(data = annot,
                # aes(label = Product_description_EN), 
                col = "black", vjust = 0, hjust = 0, nudge_x = 0.1
      )
  }
  
  g
  
}
