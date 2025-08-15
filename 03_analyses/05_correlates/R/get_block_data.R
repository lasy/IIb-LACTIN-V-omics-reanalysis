

get_block_data <- function(mae, block, PID, target_visits) {
  
  if (block == "microbiota categories")
    output <- get_microbiota_categories(mae, PID, target_visits)
 
}

get_microbiota_categories <- function(mae, PID, target_visits) {
  get_assay_wide_format(mae, "mb_categories_wide") |> 
    filter(USUBJID %in% PID, AVISITN %in% target_visits) |>
    mutate(key = str_c(USUBJID, AVISITN, sep = "_")) |>
    arrange(key) |> 
    select(key, assay) |> 
    unnest(assay) |> 
    as.data.frame() |> 
    column_to_rownames(var = "key")
}