
plot_swab_inventory <- function(coll, color){
  
  df <- coll
  df$color <- df[,color] |> unlist()
  
  df |> 
    ggplot( aes(x = SP, y = USUBJID, col = color)) +
    geom_point(alpha = 0.2) +
    facet_grid(ARM ~ AVISITN, scales = "free", space = "free") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 4),
      axis.text.y = element_text(size = 2)
    ) +
    scale_color_discrete(color)
  
}