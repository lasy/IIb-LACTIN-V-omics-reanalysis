


get_statis_loadings <- function(statis_res){
  df <- 
    statis_res$C.Co  %>% 
    as_tibble() %>% 
    mutate(
      var_block = rownames(statis_res$C.Co),
      block = statis_res$TC$T |> fct_inorder(),
      group = block |> str_remove("[0-9]"),
      visit = ifelse(str_detect(block, "[0-9]"), block |> as.character() |> parse_number(), NA),
      var = var_block |> str_remove(block |> as.character()) |>  str_remove("\\.$")
    ) 
}

plot_statis_loadings <- function(statis_res, kt, xaxis = 1, yaxis = 2){
  
  df <- get_statis_loadings(statis_res)
  
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


plot_statis_loadings_all_visits <- function(statis_res, xaxis = 1, yaxis = 2, label = "var_block"){
  
  df <- get_statis_loadings(statis_res)
  
  df$xaxis <- df[,xaxis] |> unlist()
  df$yaxis <- df[,yaxis] |> unlist()
  df$lab <- df[,label] |> unlist()
  
  perc_var <- (100 * statis_res$C.eig/sum(statis_res$C.eig)) %>% round()
  
  ggplot(df, aes(x = xaxis, y = yaxis, col = var)) +
    geom_segment(
      aes(x = 0, y = 0, xend = xaxis, yend = yaxis),
      arrow = 
        arrow(
          end = "last", type = "closed", angle = 20, length = unit(0.1, "inches")
        )
    ) +
    geom_text_repel(aes(label = lab), show.legend = FALSE) +
    coord_fixed(ratio = statis_res$C.eig[yaxis]/statis_res$C.eig[xaxis]) +
    xlab(str_c("STATIS latent component ", xaxis, " (",perc_var[xaxis],"%)")) +
    ylab(str_c("latent component ", yaxis, " (",perc_var[yaxis],"%)")) 
  
}
