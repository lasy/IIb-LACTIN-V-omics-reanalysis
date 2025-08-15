
plot_CC_cor_circle <- function(xy = list(x = NULL, y = NULL), 
                               cancor_object, d1 = 1, d2 = 2, show_arrows = TRUE){
  x <- xy[[1]] %>% as.matrix()
  y <- xy[[2]] %>% as.matrix()
  # we compute the canonical variables values
  CC_x <- x[,1:nrow(cancor_object$xcoef)] %*% cancor_object$xcoef
  CC_y <- y[,1:nrow(cancor_object$ycoef)] %*% cancor_object$ycoef
  # we compute the correlation between the original variables and the canonical variables of interest
  corr <- 
    bind_rows(
      tibble(
        variable = colnames(x),
        xaxis = cor(x, CC_x[,d1]) %>% as.vector(),
        yaxis = cor(x, CC_x[,d2]) %>% as.vector(),
        table = names(xy)[1]
      ),
      tibble(
        variable = colnames(y),
        xaxis = cor(y, CC_y[,d1]) %>% as.vector(),
        yaxis = cor(y, CC_y[,d2]) %>% as.vector(),
        table = names(xy)[2]
      )
    ) 
  
  g <- 
    ggplot(corr, aes(x = xaxis, y = yaxis, col = table)) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray90") 
  
  if (show_arrows) {
    g <- 
      g +
      geom_segment(
        xend = 0, yend = 0,
        arrow = arrow(end = "first", type = "closed", angle = 20, length = unit(0.1, "inches"))
      ) 
  } else{
    g <- 
      g +
      geom_vline(xintercept = 0, col = 'gray90', linewidth = 1) +
      geom_hline(yintercept = 0, col = "gray90", linewidth = 1) +
      geom_point() 
  }
  
  g <- 
    g +
    geom_text_repel(aes(label = variable), show.legend = FALSE) +
    coord_fixed(xlim = c(-1,1), ylim = c(-1,1)) +
    # guides(col = "none") +
    xlab(str_c("Correlation with canonical var. ", d1, 
               "\n(cor = ",cancor_object$cor[d1] %>% round(.,2),")")) +
    ylab(str_c("Correlation with canonical var. ", d2, 
               "\n(cor = ",cancor_object$cor[d2] %>% round(.,2),")"))
  
  g
  
}
