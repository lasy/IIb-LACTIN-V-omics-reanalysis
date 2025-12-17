
plot_topic_betas <- function(mae, assayname, threshold = 1/100, exclude_lacto_topics = FALSE, direction = "v", topic_label = "topic") {
  
  beta_long <- get_beta_long(mae, assayname) 
  
  if (exclude_lacto_topics) {
    beta_long <- 
      beta_long |> 
      filter(!topic_subcst_matching_label %in% c("I", "II", "III", "V", "VI"))
  }
  
  # making taxa labels prettier or shorter or more human-friendly
  beta_long <- 
    beta_long |> 
    mutate(
      taxa = 
        taxa |> 
        str_replace("Candidatus", "Ca.") |> 
        str_replace("vaginae", "v.") |> 
        str_replace("sp_", "sp. ") |>
        str_replace("_","/")
    )
  
  # re-ordering taxa labels by topic
  beta_long <- 
    beta_long |> 
    arrange(taxa, -prop) |> 
    group_by(taxa) |> 
    mutate(max_topic = topic[1]) |> 
    ungroup() |> 
    arrange(max_topic, -prop) |> 
    mutate(taxa = taxa |> factor(levels = unique(taxa)) |> fct_rev())

  scale_size_title <- "Proportion in topic"
  
  beta_long$x <- beta_long[[topic_label]]
  
  
  if (direction == "h") {
    beta_long <- 
      beta_long |> 
      mutate(
        taxa = taxa |> fct_rev(),
        x = x |> fct_rev()
      )
    # scale_size_title <- scale_size_title |> str_replace("\n", " ")
  }
  
  g <- 
    beta_long |> filter(prop > threshold) |> 
    ggplot() + 
    aes(x = x, y = taxa, size = prop, col = topic) +
    geom_point() +
    scale_size(
      scale_size_title, 
      range = c(0, 6), limits = c(0, 1), 
      breaks = c(1/100, 1/10, 1/4, 1/2, 1),
      labels = c("1/100", "1/10","1/4","1/2","1")
      ) +
    ylab("") + 
    scale_x_discrete(
      ""
    ) +
    scale_color_manual(values = get_topic_colors(levels(beta_long$topic_subcst_matching_label |> fct_drop()))) +
    guides(col = "none") # size = "none"
  
  # ,
  # breaks = levels(beta_long$topic),
  # limits = levels(beta_long$topic |> fct_drop()),
  # labels = levels(beta_long[[topic_label]])
  
  
  if (direction == "h") {
    g <- 
      g +
      coord_flip() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  } else {
    g <- g + theme(legend.title.position = "top")
  }
  
  g
  
}
