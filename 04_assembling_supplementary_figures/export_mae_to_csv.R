

export_mae_to_csv <- function(mae, dir){
  if(!fs::dir_exists(dir)){fs::dir_create(dir)}
  
  # export the MAE colData
  write_csv(mae@colData |> as.data.frame() |> rownames_to_column("uid"), file = str_c(dir, "00_colData.csv"))
  
  # export the sampleMap
  if (any(mae@sampleMap$primary != mae@sampleMap$colname)) {
    write_csv(mae@sampleMap, file = str_c(dir, "00b_sampleMap.csv"))
  }
  
  # export the SE data
  iwalk(
    MultiAssayExperiment::experiments(mae) |> names(), 
    export_mae_se_to_csv, mae = mae, dir = dir
  )
  
  # export the metadata
  if (length(mae@metadata) > 0){export_metadata_to_csv(mae, dir)}
  
}




export_mae_se_to_csv <- function(exp_name, exp_nb, mae, dir){
  cat(exp_name, "\n")
  exp_dir <- 
    str_c(
      dir, 
      exp_nb |> str_pad(width = 2, pad = "0", side = "left"), "_", 
      exp_name, "/"
    )
  if (!fs::dir_exists(exp_dir)) {fs::dir_create(exp_dir)}
  se <- mae[[exp_name]]
  assay_names <- se@assays |> names()
  if (is.null(assay_names)) {
    write_csv(
      x = se |> assay() |> t() |> as.data.frame() |> rownames_to_column("uid"), 
      file = str_c(exp_dir, "01_",exp_name,".csv")
    )
  } else {
    iwalk(
      assay_names,
      function(assay_name, assay_nb, se){
        write_csv(
          x = se |> assay(assay_name) |> t() |> as.data.frame() |> rownames_to_column("uid"), 
          file = str_c(exp_dir, assay_nb |> str_pad(width = 2, pad = "0"), "_", assay_name, ".csv")
        )
      },
      se = se
    )
  }
  
}


