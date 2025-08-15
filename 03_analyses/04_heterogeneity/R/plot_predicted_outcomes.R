
plot_predicted_outcomes <- function(pred_outcomes){
  
  pred_outcomes |> 
    group_by(USUBJID) |>
    mutate(
      pred_value_correct = predicted_value[predicted_outcome == true_outcome],
      pred_outcome = predicted_outcome[predicted_value == max(predicted_value)]
    ) |>
    ungroup() |> 
    arrange(pred_outcome, -pred_value_correct) |> 
    mutate(USUBJID = USUBJID |> factor(levels = unique(USUBJID))) |> 
    ggplot(
      aes(x = predicted_outcome, y = USUBJID |> fct_rev(), 
          fill = predicted_value)
    ) +
    geom_tile() +
    facet_grid(true_outcome ~ ., scales = "free", space = "free") +
    xlab("Predicted outcome") +
    ylab("Participants") +
    scale_fill_gradient("Predicted\nvalue",low = "white", high = "steelblue2") +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 1),
      axis.text.y = element_blank(),
      strip.text.y = element_text(angle = 0, hjust = 0),
      legend.text = element_blank()
    ) 
}

get_confusion_matrix <- function(pred_outcomes){
  pred_outcomes |> 
    group_by(USUBJID, true_outcome) |> 
    arrange(USUBJID, -predicted_value) |> 
    slice_head(n = 1) |> 
    ungroup() |> 
    count(true_outcome, predicted_outcome) |> 
    group_by(true_outcome) |>
    mutate(pct = 100 * n / sum(n), label = str_c(n, "\n(", round(pct), "%)"))
}



plot_confusion_matrix <- function(pred_outcomes){
  
  confusion_matrix <- get_confusion_matrix(pred_outcomes)
  
  ggplot(confusion_matrix, 
         aes(x = predicted_outcome, y = true_outcome |> fct_rev(), 
             fill = pct)) +
    geom_tile() +
    geom_text(aes(label = label), size = 3) + 
    scale_fill_gradient(low = "white", high = "steelblue3", limits = c(0, 100)) +
    guides(fill = "none") +
    xlab("Predicted outcome") +
    ylab("True outcome") +
    coord_fixed() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}





