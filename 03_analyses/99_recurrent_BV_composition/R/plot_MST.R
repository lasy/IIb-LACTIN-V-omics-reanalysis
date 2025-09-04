

plot_MST <- function(distances, sample_data){
  gr <- 
    igraph::graph_from_adjacency_matrix(
      adjmatrix = distances |> as.matrix(),  
      mode = "undirected", weighted = TRUE
    )
  minimumST <- igraph::mst(gr)
  igraph::V(minimumST)$USUBJID <- sample_data$USUBJID
  igraph::V(minimumST)$AVISITN <- sample_data$AVISITN
  
  gnet <- ggnetwork::ggnetwork(minimumST)
  digits <- 4
  gnet <- 
    gnet |>  
    mutate(
      x = x |> round(digits),
      y = y |> round(digits),
      xend = xend |> round(digits),
      yend = yend |> round(digits)
    )
  
  gnet <- 
    gnet |> 
    distinct() |>  
    left_join(
      gnet |> 
        select(x, y, USUBJID) |> 
        distinct() |> 
        dplyr::rename(xend = x, yend = y, USUBJID_end = USUBJID),
      by = join_by(xend, yend)
    ) |>  
    mutate(`Same participant` = (USUBJID == USUBJID_end)) 
  
  ggplot(gnet, aes(x = x, y = y, xend = xend, yend = yend))+
    ggnetwork::geom_edges(aes(color = `Same participant`), curvature = 0.0) +
    scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "gray80")) +
    ggnewscale::new_scale_color() +
    ggnetwork::geom_nodes(aes(color = USUBJID, shape = AVISITN |> get_visit_labels())) + # 
    ggnetwork::theme_blank() +
    scale_shape_manual("", values = c(1, 16)) +
    guides(col = "none") +
    theme(legend.position = "bottom")
  
}

plot_MST_v1 <- function(distances, sample_data){
  gr <- 
    igraph::graph.adjacency(
      adjmatrix = distances %>% as.matrix(),  
      mode = "undirected", weighted = TRUE
    )
  minimumST <- igraph::mst(gr)
  V(minimumST)$USUBJID <- sample_data$USUBJID
  V(minimumST)$AVISITN <- sample_data$AVISITN
  
  gnet <- ggnetwork::ggnetwork(minimumST)
  digits <- 4
  gnet <- 
    gnet %>% 
    mutate(
      x = x %>% round(., digits),
      y = y %>% round(., digits),
      xend = xend %>% round(., digits),
      yend = yend %>% round(., digits)
    )
  
  gnet <- 
    gnet %>% 
    distinct() %>% 
    left_join(
      gnet %>% 
        select(x, y, USUBJID) %>% 
        distinct() %>% 
        rename(xend = x, yend = y, USUBJID_end = USUBJID),
      by = join_by(xend, yend)
    ) %>% 
    mutate(`Same participant` = (USUBJID == USUBJID_end)) 
  
  ggplot(gnet, aes(x = x, y = y, xend = xend, yend = yend))+
    geom_edges(aes(color = `Same participant`), curvature = 0.0) +
    scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "gray80")) +
    ggnewscale::new_scale_color() +
    geom_nodes(aes(color = USUBJID)) + # 
    theme_blank() +
    guides(col = "none") +
    theme(legend.position = "bottom")
  
}