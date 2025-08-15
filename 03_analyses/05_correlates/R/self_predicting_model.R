
self_predicting_model <- function(mod_res){
  
  summary_by_fold <- 
    map(1:mod_res$mbplsda_par$cv_nrepet, 
        dummy_self_predict, 
        mod = mod_res,
        prop = mod_res$mbplsda_par$cv_prop) |> 
    bind_rows()
  
  
  summary <- 
    summary_by_fold |> 
    mutate(`error rate` = 1 - accuracy) |> 
    pivot_longer(cols = c(accuracy, `error rate`, `average F1 score`), names_to = "metric", values_to = "value") |>
    group_by(metric, set) |> 
    summarize(
      n_fold = n(),
      dim = NA,
      mean = mean(value),
      sd = sd(value),
      median = median(value),
      IQR_lo = quantile(value, 0.25),
      IQR_hi = quantile(value, 0.75),
      .groups = "drop"
    ) |> 
    mutate(
      `95%CI_lo` = mean - qt(0.975, n_fold - 1) * sd / sqrt(n_fold),
      `95%CI_hi` = mean + qt(0.975, n_fold - 1) * sd / sqrt(n_fold)
    ) |> 
    select(metric, set, dim, n_fold, mean, sd, `95%CI_lo`, `95%CI_hi`, median, IQR_lo, IQR_hi)
  list(cv = list(summary = summary, summary_by_fold = summary_by_fold))
}

dummy_self_predict <- function(i, mod_res, prop){
  actual <- mod_res$output_fct
  prev <- mod_res$inputs_raw$`Coloniz. cat. prev. v.`
  prev <- prev |> colnames() |> extract(apply(prev, 1, which.max)) |> factor(levels = levels(actual))
  N <- length(actual)
  
  ic <- caret::createDataPartition(actual, p = prop, list = FALSE) |> as.vector()
  iv <- setdiff(1:N, ic)
  model_cal <- table(prev = prev[ic], actual = actual[ic]) |> as.matrix() |> apply(1, function(x) nnet::which.is.max(x)) 
  model_cal <- levels(actual)[model_cal] |> factor(levels = levels(actual))
  dummy_pred <- model_cal[prev |> as.numeric()]
  # dummy_pred <- prev
  random_pred <- sample(levels(actual), length(iv), prob = table(actual[ic]), replace = TRUE) |> factor(levels = levels(actual))
  bind_rows(
    tibble(
      accuracy = mean(actual == dummy_pred),
      `average F1 score` = ave_F1_score(actual, dummy_pred),
      set = "whole dataset"
    ),
    tibble(
      accuracy = mean(actual[ic] == dummy_pred[ic]),
      `average F1 score` = ave_F1_score(actual[ic], dummy_pred[ic]),
      set = "calibration"
    ),
    tibble(
      accuracy = mean(actual[iv] == dummy_pred[iv]),
      `average F1 score` = ave_F1_score(actual[iv], dummy_pred[iv]),
      set = "validation"
    ) ,
    tibble(
      accuracy = mean(actual[iv] == random_pred),
      `average F1 score` = ave_F1_score(actual[iv], random_pred),
      set = "random predictions"
    )
  ) |> 
    mutate(i = i, set = set |> fct_inorder())
  

}


ave_F1_score <- function(actual, predicted){
  caret::confusionMatrix(predicted, actual)$byClass |> 
    extract(,"F1") |> 
    replace_na(0) |> 
    mean()
}