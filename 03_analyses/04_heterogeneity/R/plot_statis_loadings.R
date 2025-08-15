
plot_statis_loadings <- function(statis_res, kt, xaxis = 1, yaxis = 2){
  
  df <- 
    statis_res$C.Co  %>% 
    as_tibble() %>% 
    mutate(
      var = rownames(statis_res$C.Co),
      block = rep(kt$blo %>% names(), kt$blo),
      block = block %>% factor(., levels = unique(block))
    ) 
  
  df$xaxis <- df[,xaxis] %>% unlist()
  df$yaxis <- df[,yaxis] %>% unlist()
  
  perc_var <- (100 * statis_res$C.eig/sum(statis_res$C.eig)) %>% round()
  
  ggplot(df, aes(x = xaxis, y = yaxis, col = block)) +
    geom_segment(aes(x = 0, y = 0, xend = xaxis, yend = yaxis),
                 arrow = arrow(end = "last", type = "closed", angle = 20, length = unit(0.1, "inches"))) +
    geom_text_repel(aes(label = var), show.legend = FALSE) +
    coord_fixed(ratio = statis_res$C.eig[yaxis]/statis_res$C.eig[xaxis]) +
    xlab(str_c("STATIS latent component ", xaxis, " (",perc_var[xaxis],"%)")) + ylab(str_c("latent component ", yaxis, " (",perc_var[yaxis],"%)")) 
  
}

plot_statis_eig <- function(statis_res){
  df <- 
    tibble(eig = statis_res$C.eig) %>% 
    mutate(
      component = row_number(), 
      `perc. var.` = eig / sum(eig)
    )
  
  ggplot(df, aes(x = component, y = `perc. var.`)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent)
}
