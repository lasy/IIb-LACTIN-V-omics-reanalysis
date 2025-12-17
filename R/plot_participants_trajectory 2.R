
plot_participants_trajectory <- function(df, feature_name = "Taxa", n_features = 15){
  
  selected_features <- 
    df %>% 
    group_by(feature) %>% 
    summarize(mean = mean(prop), .groups = "drop") %>%
    arrange(-mean) %>% 
    slice_head(n = n_features) %>% 
    mutate(keep = TRUE)
  
  feature_levels <- c(selected_features$feature %>% fct_drop() %>% levels(), "Others")
  
  new_df <- 
    df %>% 
    left_join(selected_features, by = "feature") %>% 
    mutate(feature = case_when(keep ~ feature, TRUE ~ "Others")) %>% 
    group_by(Barcode, USUBJID, AVISITN, feature) %>% 
    summarize(prop = sum(prop), .groups = "drop") %>% 
    mutate(feature = feature %>% factor(., levels = feature_levels))
  
  new_df <- 
    new_df %>% 
    mutate(Visit = AVISITN %>% get_visit_labels(., long = TRUE))
  
  if (str_detect(feature_name, "[tT]axa")) {
    color_values <- get_taxa_colors(levels(new_df$feature))
  } else if (any(str_detect(feature_name, c("[tT]opic", "[Ss]ub-communit")))) {
    color_values <- get_topic_colors(levels(new_df$feature))
  }
  
  
  ggplot(new_df, aes(x = Visit, y = prop, fill = feature)) +
    geom_bar(stat = "identity", col = "white", linewidth = 0.5) +
    facet_grid(USUBJID ~ .) +
    scale_fill_manual(feature_name, values = color_values) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}