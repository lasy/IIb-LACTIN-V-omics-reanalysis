
compute_BC_between_successive_visits <- function(mae, assayname, v = c(0:4,7)){
  pairs <- tibble(v1 = v[1:(length(v) - 1)], v2 = v[2:length(v)])
  map(
    .x = 1:nrow(pairs), 
    .f = 
      function(x, mae, assayname, pairs) 
        compute_BC_between_visits(mae, assayname, v1 = pairs$v1[x], v2 = pairs$v2[x]),
    mae = mae,
    assayname = assayname,
    pairs = pairs
  ) |> bind_rows()
}

compute_BC_between_visits <- function(mae, assayname, v1 = 0, v2 = 7){
  
  ASVs <- MultiAssayExperiment::assay(mae, assayname)
  
  df <- MultiAssayExperiment::colData(mae) |> as.data.frame() |> as_tibble()
  df1 <- df %>% filter(AVISITN == v1) |>  filter(Barcode %in% colnames(ASVs))
  df2 <- df %>% filter(AVISITN == v2) |>  filter(Barcode %in% colnames(ASVs))
  df_common <- 
    inner_join(
      df1 |> select(USUBJID, Barcode), 
      df2 |> select(USUBJID, Barcode),
      by = "USUBJID", suffix = c("_v1","_v2")
    )
  
  ASV1 <- ASVs[,df_common$Barcode_v1] |> t() |> as_tibble() |> as.matrix()
  ASV2 <- ASVs[,df_common$Barcode_v2] |> t() |> as_tibble() |> as.matrix()
  
  BC <- 1 - 2 * rowSums(pmin(ASV1, ASV2)) / rowSums(ASV1 + ASV2)
  
  df_common %>% mutate(v1 = v1, v2 = v2, BC = BC)
}