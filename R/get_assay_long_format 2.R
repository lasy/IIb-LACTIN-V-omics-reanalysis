
get_assay_long_format <- 
  function(mae, assayname, add_colData = TRUE, add_rowData = FALSE, feature_name = "feature", values_name = "value") {
    
    a <- MultiAssayExperiment::assay(mae, assayname) %>% t()
    a_long <- 
      a %>% 
      as.data.frame() %>% 
      mutate(Barcode = rownames(a)) %>% 
      pivot_longer(
        cols = -Barcode, 
        names_to = feature_name, 
        values_to = values_name
      ) 
    
    a_long[,feature_name] <- 
      a_long[, feature_name] %>% 
      unlist() %>% 
      factor(., levels = colnames(a)) # %>% sort()
    
    if (add_colData) {
      a_long <- 
        a_long %>% 
        left_join(
          ., 
          MultiAssayExperiment::colData(mae) %>% as.data.frame(), 
          by = "Barcode"
        )
    }
    
    if (add_rowData){
      a_long <- 
        a_long |> 
        left_join(
          SummarizedExperiment::rowData(mae[[assayname]]) |> 
           as.data.frame() |> 
            rownames_to_column(feature_name) |>
            select(all_of(feature_name), everything()), 
          by = feature_name
        )
    }
    
    a_long
  }