format_predictions <- function(predictions){
  pred_output <- predictions$PredY.max |> as_tibble() |> select(starts_with("Pred_"))
  colnames(pred_output) <- colnames(pred_output) |> str_remove("Pred_")
  pred_fct <- colnames(pred_output)[apply(pred_output, 1, which.max)] |> factor(levels = colnames(pred_output))
  list(pred_output = pred_output, pred_fct = pred_fct)
}
