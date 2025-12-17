
get_assay_wide_format <- function(mae, assayname, add_colData = TRUE){
  a <- MultiAssayExperiment::assay(mae, assayname)
  a <- a %>% t() %>% as.data.frame()
  a_wide <- 
    tibble(
      Barcode = rownames(a),
      assay = a %>% as_tibble()
    )
  if (add_colData) {
    a_wide <- 
      a_wide %>% 
      left_join(
        .,
        MultiAssayExperiment::colData(mae) %>% as.data.frame(),
        by = "Barcode"
      )
  }
  a_wide
}
