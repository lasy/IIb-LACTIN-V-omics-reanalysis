
get_mtb_relbipc_boot_values <- function(boot = boot){
  (boot$res$bipc / boot$res$rel_block_var) |> 
    as_tibble() |> 
    mutate(boot_nb = row_number()) |> 
    pivot_longer(cols = -boot_nb, names_to = "block", values_to = "rel_BIPC") |> 
    mutate(block = block |> fct_inorder()) 
}


get_mtb_relbipc <- function(res = res, boot = NULL){
  obs_relbipc <- 
    tibble(
      block = res$bipc |> rownames() |> fct_inorder(),
      bipc = res$bipc[, res$nf]
    ) |> 
    left_join(
      res$TC |> 
        as_tibble() |> 
        mutate(block = T |> fct_inorder(), var_var = apply(res$tabX, 2, var)) |> 
        group_by(block) |> summarize(block_var = sum(var_var)) |> 
        mutate(rel_block_var = block_var / sum(block_var)),
      by = join_by(block)
    ) |> 
    mutate(rel_BIPC = bipc / rel_block_var)
  
  if (!is.null(boot)){
    boot_relbipc <- 
      boot |> 
      get_mtb_relbipc_boot_values()|> 
      group_by(block) |> 
      summarize(
        median_rel_BIPC = median(rel_BIPC),
        min_rel_BIPC = min(rel_BIPC),
        max_rel_BIPC = max(rel_BIPC),
        Q2.5_rel_BIPC = quantile(rel_BIPC, 0.025),
        Q97.5_rel_BIPC = quantile(rel_BIPC, 0.975)
      ) |> 
      ungroup() 
    relbipc <- obs_relbipc |> left_join(boot_relbipc, by = join_by(block))
  }
  else{
    relbipc <- obs_relbipc
  }
  relbipc
}

plot_mtb_relbipc <- function(res = res, boot = NULL){
    relbipc <- get_mtb_relbipc(res = res, boot = boot) 
    if (is.null(boot)) relbipc <- relbipc |> mutate(Q2.5_rel_BIPC = rel_BIPC, Q97.5_rel_BIPC = rel_BIPC)

    ggplot(relbipc) +
    aes(x = block, y = rel_BIPC, col = block) +
    geom_hline(yintercept = 1, col = "gray") +
    geom_linerange(aes(ymin = Q2.5_rel_BIPC, ymax = Q97.5_rel_BIPC), size = 2, alpha = 0.5, lineend = "round") +
    geom_point(size = 3) +
    guides(col = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("") + ylab("Relative BIPC")
    
}


get_mtb_relbipc_table <- function(res, boot = NULL){
  get_mtb_relbipc(res = res, boot = boot) |> 
    select(block, rel_BIPC, Q2.5_rel_BIPC, Q97.5_rel_BIPC) |> arrange(-rel_BIPC) |> 
    dplyr::rename(
      Block = block, 
      `Relative Block Importance` = rel_BIPC, 
      `95% CI lower` = Q2.5_rel_BIPC, 
      `95% CI upper` = Q97.5_rel_BIPC
      ) |> 
    gt() |> 
    cols_align(align = "right", col = Block) |> 
    fmt_number(decimals = 2)
}

