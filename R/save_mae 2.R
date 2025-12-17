
save_mae <- function(mae, output_dir) {
  if (!fs::dir_exists(output_dir)) dir_create(output_dir)
  mae_file_name <- 
    str_c(output_dir, "mae_", Sys.Date() |> str_remove_all("-"),".RDS")
  
  last_mae <- load_latest_mae(output_dir, silent = TRUE)
  if (!is.null(last_mae)) {
    if (identical(last_mae, mae)) {
      message("MAE is identical to the last one. Not saving.")
    } else {
      message("MAE is not identical to the last one. ",
              "Saving as ", mae_file_name |> basename())
      saveRDS(mae, file = mae_file_name)
    }
  } 
  else {
    message("Saving MAE as ", mae_file_name |> basename())
    saveRDS(mae, file = mae_file_name)
  }
  invisible(mae_file_name)
}
