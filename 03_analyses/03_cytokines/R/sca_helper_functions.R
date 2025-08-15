
get_sca_loadings <- function(sca_res) {
  map(
    1:length(sca_res$blockLoadings),
    function(i, X){
      x <- X[[i]]  
      x |> 
        as_tibble() |> 
        mutate(var = rownames(x), block = names(X)[i]) |> 
        select(var,block, everything())
    },
    X = sca_res$blockLoadings
  ) |> 
    bind_rows()
}


get_sca_expl_var <- function(sca_res){
  expl_var <- tibble(comp = 1:ncomp |> factor(), `perc. var.` = sca_res$explvar)
  expl_var_by_block <- 
    sca_res |> 
    get_sca_loadings() |> 
    pivot_longer(cols = -c(var, block), names_to = "comp", values_to = "loadings") |> 
    group_by(comp, block) |> 
    summarize(var_loadings = var(loadings), .groups = "drop_last") |> 
    mutate(frac_var = var_loadings / sum(var_loadings)) |> 
    ungroup() |> 
    arrange(comp, block) |> 
    mutate(comp = comp |> parse_number() |> factor()) |> 
    left_join(expl_var |> dplyr::rename(`total perc. var.` = `perc. var.`),  by = join_by(comp)) |> 
    mutate(`perc. var.` = `total perc. var.` * frac_var) |> 
    select(block, comp, `perc. var.`, `total perc. var.`)
  list(expl_var = expl_var, expl_var_by_block = expl_var_by_block)
}


plot_sca_expl_var <- function(sca_res, by_block = TRUE, axes = 1:2){
  ev <- get_sca_expl_var(sca_res) 
  
  if (!by_block){
    ev$expl_var |> 
      mutate(selected_axes = (comp %in% axes) * 1) |> 
      ggplot(aes(x = comp, y = `perc. var.`, alpha = selected_axes)) +
      geom_col() +
      scale_alpha(range = c(0.5, 1), limits = c(0, 1)) +
      guides(alpha = "none")
  } else {
    ev$expl_var_by_block |> 
      mutate(selected_axes = (comp %in% axes) * 1) |> 
      ggplot(aes(x = comp, y = `perc. var.`, fill = block,  alpha = selected_axes)) +
      geom_col() +
      scale_alpha(range = c(0.5, 1), limits = c(0, 1)) +
      guides(alpha = "none")
  }
}



plot_sca_loadings <- function(sca_res, xaxis = 1, yaxis = 2){
  ev <- get_sca_expl_var(sca_res)
  pvar <- ev$expl_var$`perc. var.`
  loadings <- get_sca_loadings(sca_res)
  loadings$xaxis <- loadings[,str_c("Comp ", xaxis)] %>% unlist()
  loadings$yaxis <- loadings[,str_c("Comp ", yaxis)] %>% unlist()
  
  loadings |> 
    ggplot(aes(x = xaxis, y = yaxis, col = block)) +
    geom_hline(yintercept = 0, linewidth = 0.25, col = "gray50") + geom_vline(xintercept = 0, linewidth = 0.25, col = "gray50") +
    geom_segment(aes(x = 0, y = 0, xend = xaxis, yend = yaxis),
                 arrow = arrow(end = "last", type = "closed", angle = 20, length = unit(0.1, "inches"))) +
    geom_text_repel(aes(label = var), show.legend = FALSE) +
    coord_fixed(pvar[yaxis]/pvar[xaxis]) +
    xlab(str_c("Comp ", xaxis, " (", pvar[xaxis] |> round(),"% var. explained)")) + 
    ylab(str_c("Comp ", yaxis, " (", pvar[yaxis] |> round(),"% var. explained)"))
}


plot_sca_scores <- function(scores, sca_res, xaxis = 1, yaxis = 2){
  ev <- get_sca_expl_var(sca_res)
  pvar <- ev$expl_var$`perc. var.`
  scores |>
    ggplot(aes(x = `Comp 1`, y = `Comp 2`, col = ARM)) +
    geom_hline(yintercept = 0, linewidth = 0.25, col = "gray50") + geom_vline(xintercept = 0, linewidth = 0.25, col = "gray50") +
    geom_point(size = 0.5, alpha = 0.7) +
    coord_fixed(pvar[yaxis]/pvar[xaxis]) +
    xlab(str_c("Comp ", xaxis, " (", pvar[xaxis] |> round(),"% var. explained)")) + 
    ylab(str_c("Comp ", yaxis, " (", pvar[yaxis] |> round(),"% var. explained)"))
}


get_sca_scores <- function(sca_res){
  sca_res$scores[, 1:ncol(sca_res$scores)] |>
    as_tibble() 
}