
get_relative_abundance_assay <- function(mae, assay_name) {
  a <- MultiAssayExperiment::assay(mae, assay_name)
  b <- (t(a)/colSums(a)) %>% t()
  new_assay_name <- str_c(assay_name, "_p")
  new_assay = list()
  new_assay[[new_assay_name]] <-
    SummarizedExperiment::SummarizedExperiment(
      assay = b,
      rowData = SummarizedExperiment::rowData(mae[[assay_name]]),
      colData = SummarizedExperiment::colData(mae[[assay_name]])
    )
  new_assay
}
