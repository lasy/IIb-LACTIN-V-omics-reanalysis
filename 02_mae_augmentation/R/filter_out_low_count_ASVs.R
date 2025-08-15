
filter_out_low_count_ASVs <- 
  function(mae, assay_name, min_tot_count = 50, min_n_samples = 2, min_counts_per_sample = 5) {
    a <- MultiAssayExperiment::assay(mae, assay_name)
    total_counts <- rowSums(a)
    n_samples <- rowSums(a > min_counts_per_sample)
    j <- which((total_counts >= min_tot_count) & (n_samples >= min_n_samples))
    b <- a[j,]
    
    df <- 
      tibble(
        ASV_name = rownames(a), 
        total_counts = total_counts, 
        n_samples = n_samples
        ) |>
      mutate(i = row_number(), Included = ifelse(i %in% j,"Yes","No"))
    
    g_diagnosis <- 
      ggplot(df, aes(x = total_counts, y = n_samples)) +
      geom_point(aes(col = Included), size = 0.5, alpha = 0.5) +
      ggrepel::geom_text_repel(
        data = df |> arrange(-total_counts) |> slice_head(n = 3), 
        aes(label = ASV_name)
      ) +
      scale_x_log10("total number of reads across all samples") +
      scale_y_log10(str_c("number of samples with at least ",min_counts_per_sample," reads")) +
      labs(caption = "Each dot is an ASV")
    
    
    new_assay_name <- str_c(assay_name, "_filtered")
    new_assay = list()
    new_assay[[new_assay_name]] <-
      SummarizedExperiment::SummarizedExperiment(
        assay = b,
        rowData = SummarizedExperiment::rowData(mae[[assay_name]])[j,],
        colData = SummarizedExperiment::colData(mae[[assay_name]])
      )
    list(SE = new_assay, plot = g_diagnosis)
  }
