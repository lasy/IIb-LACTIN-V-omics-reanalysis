load_latest_mae <- function(dir, silent = FALSE){
  last_mae_file <- fs::dir_ls(dir, regexp = "mae_[0-9]*.RDS") |> tail(1)
  if (length(last_mae_file) > 0) {
    if (!silent) message("Loading ", last_mae_file |> basename())
    last_mae <- readRDS(last_mae_file)
  } else {
    if (!silent) message("No MAE file found in ", dir)
    last_mae <- NULL
  }
  last_mae
}