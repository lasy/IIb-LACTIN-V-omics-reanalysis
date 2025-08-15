
plot_marginal_inputs <- function(mod_res){
  
  inputs <- 
    mod_res$inputs_raw |> 
    map(~ .x |> rownames_to_column("key") |>  as_tibble() |> pivot_longer(-key, names_to = "variable")) |> 
    bind_rows() |> 
    left_join(
      map(1:length(mod_res$inputs_raw), 
          ~ tibble(block = names(mod_res$inputs_raw)[.x], variable = colnames(mod_res$inputs_raw[[.x]]))
      ) |> bind_rows(),
      by = join_by(variable)
    ) |> 
    filter(block %in% names(mod_res$inputs)) |> 
    mutate(variable = variable |> fct_inorder(), block = block |> fct_inorder())
  
  inputs |> 
    ggplot(aes(x = value, fill = block)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ variable, scales = "free") +
    guides(fill = "none")
}