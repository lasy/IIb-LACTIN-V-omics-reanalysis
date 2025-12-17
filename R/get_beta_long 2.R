
get_beta_long <- function(mae, assayname) {
  beta <- mae[[assayname]]@metadata$beta
  beta |>  
    as.data.frame() |> 
    set_colnames(colnames(beta)) |> 
    mutate(topic = rownames(beta) |> fct_inorder()) |> 
    pivot_longer(-topic, names_to = "taxa", values_to = "prop") |> 
    left_join(
      SummarizedExperiment::rowData(mae[[assayname]]) |> 
        as.data.frame() |> 
        rownames_to_column("topic") |> 
        mutate(topic = topic |> fct_inorder()),
      by = "topic"
    )
}
