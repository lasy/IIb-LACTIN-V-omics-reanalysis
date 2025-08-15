
make_topic_SE <- function(mae, assay_name, lda_model) {

  assay <- lda_model$gamma %>% t()
  beta <- lda_model$beta %>% exp()
  
  assay_rowdata <- 
    beta |> 
    as.data.frame() |> 
    rownames_to_column("topic_subcst_matching_label") |> 
    mutate(
      topic_subcst_matching_label = 
        topic_subcst_matching_label |> fct_inorder()
      ) |> 
    pivot_longer(
      cols = -topic_subcst_matching_label,
      names_to = "taxon",
      values_to = "prop"
    ) |> 
    group_by(topic_subcst_matching_label) |> 
    arrange(topic_subcst_matching_label, -prop) |> 
    slice_head(n = 1) |> 
    ungroup() |> 
    mutate(
      topic_number = 1:n(),
      topic_label = 
        case_when(
          prop == 1 ~ taxon |> str_replace("Lactobacillus", "L."),
          str_detect(taxon, "crispatus|iners|jensenii") ~ taxon |> str_replace("Lactobacillus", "L.") |> str_c(" topic"),
          str_detect(taxon, "Lactobacillus") ~ "Other L.",
          TRUE ~ 
            taxon |> 
            str_replace("Prevotella", "P.") |> 
            str_replace("Gardnerella swidsinskii_leopoldii", "G. s./l.") |>
            str_replace("Candidatus Lachnocurva vaginae", "Ca. L. v.") |>
            str_c(" topic")
        ) |> fct_inorder()
    ) |> 
    select(topic_number, topic_label, topic_subcst_matching_label) |> 
    as.data.frame()
  
  rownames(assay) <- rownames(assay_rowdata)
  rownames(beta) <- rownames(assay_rowdata)

  topic_assay <- list()
  topic_assay[[assay_name]] <-
    SummarizedExperiment::SummarizedExperiment(
      assay = assay,
      rowData = assay_rowdata,
      metadata = list(beta = beta)
    )

  topic_assay
}
