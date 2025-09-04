
plot_s_comparison <- function(res, title){
  g <- 
    ggplot(res, aes(x = s, y = s_hat, col = variable)) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point() +
    xlab("true s") + ylab("estimated s") + ggtitle(title) +
    guides(col = "none")
  if ("q5" %in% colnames(res))
    g <- g + geom_segment(aes(xend = s, y = q5, yend = q95))
  g
}
