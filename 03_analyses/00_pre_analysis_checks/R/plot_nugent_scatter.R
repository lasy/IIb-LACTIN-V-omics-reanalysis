
plot_nugent_scatter <- function(clin, xvar, yvar) {
  
  df <- clin
  df$x <- clin[,xvar] %>% unlist()
  df$y <- clin[,yvar] %>% unlist()
  
  df <- 
    df %>% 
    group_by(x, y) %>% 
    mutate(
      n_in_bin = row_number(),
      N_in_bin = n()
    ) %>% 
    ungroup() %>% 
    mutate(
      max_N = max(N_in_bin)
    ) %>% 
    group_by(x, y) %>% 
    mutate(
      new_x = x + rnorm(N_in_bin, sd = ((N_in_bin-1)/(max_N-1))^(1/2)/8),
      new_y = y + rnorm(N_in_bin, sd = ((N_in_bin-1)/(max_N-1))^(1/2)/8)
    ) 
  
  ggplot(df, aes(x = new_x, y = new_y, col = N_in_bin)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_x_continuous(breaks = 0:4, minor_breaks = NULL) +
    scale_y_continuous(breaks = 0:4, minor_breaks = NULL) +
    xlab(xvar) + ylab(yvar) +
    scale_color_gradient(low = "black", high = "indianred2") +
    guides(col = "none")
  
  
}
