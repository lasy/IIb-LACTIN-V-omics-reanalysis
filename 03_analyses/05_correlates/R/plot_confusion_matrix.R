plot_confusion_matrix <- function(true_outcomes, pred_outcomes){
  
 
  res <- get_classification_performances(true_outcomes, pred_outcomes)
  
  res$conf_mat |> 
    ggplot(
      aes(x = predicted_outcome, y = true_outcome |> fct_rev(), 
          fill = pct)
    ) +
    geom_tile() +
    geom_text(aes(label = label), size = 3) + 
    scale_fill_gradient(low = "white", high = "steelblue3", limits = c(0, 100)) +
    guides(fill = "none") +
    xlab("Predicted outcome") +  scale_x_discrete(drop = FALSE) +
    ylab("True outcome") + scale_y_discrete(drop = FALSE) +
    labs(caption = str_c("Average F1: ", res$performances$aveF1[1] |> round(2),", weighted F1: ", res$performances$wF1[1] |> round(2))) +
    coord_fixed() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}


get_classification_performances <- function(true_outcomes, pred_outcomes){
  conf_mat <- 
    tibble(true_outcome = true_outcomes, predicted_outcome = pred_outcomes) |> 
    dplyr::count(true_outcome, predicted_outcome) |> 
    group_by(true_outcome) |> 
    mutate(pct = 100 * n / sum(n), label = str_c(n, "\n(", round(pct), "%)")) 
  
  performances <- 
    conf_mat |> 
    group_by(true_outcome) |> 
    mutate(
      TP = ifelse(any(true_outcome == predicted_outcome), n[true_outcome == predicted_outcome], 0),
      P = sum(n),
    ) |> 
    group_by(predicted_outcome) |>
    mutate(
      PP = sum(n)
    ) |> 
    ungroup() |> 
    mutate(
      precision = TP/PP,
      recall = TP/P
    ) |> 
    filter(true_outcome == predicted_outcome) |> 
    mutate(
      F1 = 2 * precision * recall / (precision + recall),
      aveF1 = mean(F1),
      wF1 = weighted.mean(F1, w = P/sum(P))
    ) 
  
  list(conf_mat = conf_mat, performances = performances)
}
