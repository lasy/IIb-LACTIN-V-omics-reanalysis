select_opt_ncomp <- function(cv_metric_to_minimize, criteria = c("min","1se")){
  cv_metric_to_minimize |> 
    pivot_wider(id_cols = n, names_from = set, values_from = mean) |> 
    arrange(n) |> 
    mutate(
      delta_cal = 
        case_when(
          (row_number() == 1) ~ max(`random predictions`, na.rm = TRUE) - calibration[1],
          TRUE ~ lag(calibration) - calibration
        ),
      criteria_to_minimize = validation - delta_cal/10
      ) |> 
    filter(criteria_to_minimize == min(criteria, na.rm = TRUE)) |> 
    pull(n) |> 
    head(1)
}