
plot_slda_beta <- function(slda_model, order_by = c("props","taxa", "coef")){
  
  order_by <- match.arg(order_by)
  
  beta <- 
    (slda_model$topics/rowSums(slda_model$topics)) %>% 
    as.data.frame() %>% 
    mutate(
      k = row_number(),
      coef = slda_model$model$coefficients
    ) %>% 
    pivot_longer(
      cols = -c(k, coef), names_to = "vocab", values_to = "prop"
    ) %>% 
    filter(prop > 0) %>% 
    left_join(., vocab_dict, by = join_by(vocab)) 
  
  if (order_by == "props") {
    beta <- 
      beta %>% 
      group_by(taxa) %>% 
      arrange(taxa, -prop) %>% 
      mutate(
        max_topic = k[1],
        mean_topic = weighted.mean(k, prop),
        main_topic = round(mean_topic)
      ) %>% 
      ungroup() %>% 
      arrange(max_topic, -prop) %>% 
      mutate(taxa = taxa %>% factor(., levels = unique(taxa)))
  } else if (order_by == "taxa") {
    beta <- 
      beta %>% 
      arrange(taxa) %>% 
      mutate(taxa = taxa %>% factor(., levels = unique(taxa)))
  } else if (order_by == "coef") {
    beta <- 
      beta %>% 
      arrange(taxa) %>% 
      group_by(taxa) %>% 
      mutate(contrib = sum(prop * coef)) %>% 
      ungroup() %>% 
      arrange(contrib) %>% 
      mutate(taxa = taxa %>% factor(., levels = unique(taxa)))
  }
 
  
  
  ggplot(beta %>% filter(prop > 0.001), 
         aes(x = k %>% factor(), y = taxa %>% fct_rev())) +
    geom_point(aes(col = coef, size = prop)) +
    scale_color_gradient2() +
    ylab("") + xlab("topic") 
  
}
