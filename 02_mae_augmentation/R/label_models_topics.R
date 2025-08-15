
label_models_topics <- function(lda_models, tax_table, valencia_centroids_mat, distance = "YC") {
  purrr::map(
    .x = lda_models,
    .f = label_model_topics,
    tax_table = tax_table,
    valencia_centroids_mat = valencia_centroids_mat,
    distance = distance
  )
}

label_model_topics <- function(lda_model, tax_table, valencia_centroids_mat, distance = "YC") {
  
  beta_Valencia_tax <-
    ValenciaR::convert_to_Valencia_taxonomy(
      lda_model$beta |> exp(),
      tax_table = tax_table
      )
  
  clusters <-
    ValenciaR::assign_to_Valencia_clusters(
      beta_Valencia_tax$converted_input,
      distance = distance
      )

  tmp <-
    tibble(
      topic_name = clusters$assignment$subCST %>% str_replace("I-[A-B]","I"),
      CST = clusters$assignment$CST,
      distance = clusters$distances %>% apply(., 1, min)
    ) %>%
    mutate(
      topic_name =
        case_when(
          distance > 0.5 ~ str_c(CST, "-O"),
          TRUE ~ topic_name
        )
    )

  topic_names <- tmp$topic_name

  if (any(duplicated(topic_names))) {
    tmp <-
      tmp %>%
      group_by(topic_name) %>%
      mutate(
        n = rank(distance),
        N = n(),
        topic_name_suffix =
          case_when(
            N == 1 ~ "",
            N > 1 ~ str_c(".",letters[n])
          ),
        new_topic_name =
          str_c(topic_name, topic_name_suffix)
      )  %>%
      ungroup()
    topic_names <-  tmp$new_topic_name
  }

  rownames(lda_model$beta) <- topic_names
  colnames(lda_model$gamma) <- topic_names

  lda_model
}
