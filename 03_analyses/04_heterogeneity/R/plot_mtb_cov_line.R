
plot_mtb_cov_line <- function(res, block_colors = NULL){
  input_var <- blocks_and_variables(res)
  cov <- 
    res$cov2[,1:res$nf] |> 
    as.data.frame() |> 
    rownames_to_column("block") |>
    as_tibble() |> 
    mutate(
      pretty_block = block |> str_wrap(25),
      block = block |> factor(levels = input_var$block |> levels())
      ) |> 
    arrange(block) |> 
    mutate(pretty_block = pretty_block |> factor(levels = pretty_block |> unique())) |> 
    pivot_longer(starts_with("Ax"), names_to = "latent factor", values_to = "cov2") # |> 
    # mutate(`latent factor` = `latent factor` |> str_remove("Ax") |> as.numeric())
  
  #  lY <- res$lY %>% as_tibble() %>% mutate(sampleID = rownames(res$lY))
  lY <- 
    tibble(var = rownames(res$Yco) |> factor(levels = rownames(res$Yco)), 
           res$Yco[,1:res$nf] |> as_tibble()) |> 
    pivot_longer(-var, names_to = "latent factor", values_to = "loading")
  
  g_cov <- 
  ggplot(cov, aes(x = pretty_block, y = cov2, fill = block)) +
    geom_col(width = 0.2) +
    facet_grid(`latent factor` ~ . , scales = "free_y") + 
    guides(fill = "none") +
    xlab("") +
    ylab("Covariance") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0, hjust = 0)
    ) 
  
  if (!is.null(block_colors)) {
    g_cov <- 
      g_cov +
      scale_fill_manual(values = block_colors) 
  }
  
    
  g_cov +
    
    ggplot(lY, aes(x = var, y = loading)) +
    geom_hline(yintercept = 0) +
    geom_col(width = 0.2) +
    facet_grid(`latent factor` ~ . , scales = "free_y") + 
    xlab("") + ylab("Loadings") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0, hjust = 0)
      ) +
    
    plot_layout(widths = c(2,1))
  
}
