
fit_outcome_prediction_models <- function(data = AYV, outcome = "Lc_week_12", covariate = "microbiota", use_IPW = FALSE, model = "glm", family = "quasibinomial", test_deviance = "F"){
  
  if (model != "glm") stop("not implemented yet")

  # We first format the data to unnest multivariate coviates
  formated_data <- 
    data |> 
    select(USUBJID, ARM, !!sym(outcome), !!sym(covariate)) |>
    unnest(cols = !!sym(covariate), names_sep = "_") 
  
  if (!is.null(ncol(data[[covariate]]))) {
    V_names <- str_c("`",covariate,"_", colnames(data[[covariate]]), "`")
  } else
  {
    V_names <- covariate
  }
  
  # We filter the data to include complete observations
  filtered_data <- 
    formated_data |> 
    drop_na()
  
  # We next compute propensity scores and associated weights
  if (!use_IPW) {
    filtered_data$propensity_score <- 0.5
  } else {
    filtered_data <- filtered_data |> mutate(arm = (ARM != "Placebo") * 1)
    ps_formula <- str_c("arm ~ ", V_names |> str_c(collapse = " + "))
    ps_model <- glm(ps_formula, data = filtered_data, family = "binomial")
    filtered_data$propensity_score <- predict(ps_model, type = "response")
  }
  
  filtered_data <- 
    filtered_data |> 
    mutate(
      w = 
        case_when(
          (!!sym(outcome) == 1) ~ (1 / propensity_score),
          TRUE ~ 1 / (1 - propensity_score)
        )
    )
  
  # We fit the nested models
  
  m0_formula <- str_c(outcome, " ~ ARM")
  m_formula <- str_c(outcome, " ~ ARM + ", V_names |> str_c(collapse = " + "), " + ", str_c(V_names, ":ARM") |> str_c(collapse = " + "))
  
  m0 <- glm(m0_formula, data = filtered_data, family = family, weights = w)
  m <- glm(m_formula, data = filtered_data, family = family, weights = w)
  
  heterogeneity_test <- anova(m, m0, test = test_deviance)
  
  # And return the results
  list(
    original_data = data, formated_data = formated_data, model_fit_data = filtered_data, 
    outcome = outcome, covariate = covariate, V_names = V_names, 
    m0 = m0, m = m, heterogeneity_test = heterogeneity_test
    )
}



plot_predicted_outcome <- function(models){
  tibble(
    y = models$model_fit_data |> pull(var = models$outcome) |> factor(),
    y_hat = models$m$fitted.values
  ) |> 
    ggplot(aes(x = y, y = y_hat)) + 
    geom_boxplot() + 
    geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
    xlab("Y") + ylab("predicted P(Y)")
}


predict_counterfactuals <- function(models, conf.level = 0.95){
  Z <- qnorm(1 - (1 - conf.level)/2)
  
  # We first select participants for which the covariate V is observed (not missing)
  j <- which(!is.na(models$formated_data |> pull(models$V_names[1] |> str_remove_all("`"))))
  # note formated_data contains data for all participants.
  
  
  # We next create a data frame where we replicate the dataset three times:
  # - one to hold the actual outcome
  # - one to hold the predicted outcome under LACTIN-V
  # - one to hold the predicted outcome under Placebo
  res <- 
    bind_rows(
      models$formated_data[j,] |> mutate(type = "actual"),
      models$formated_data[j,] |> mutate(type = "predicted", ARM = "LACTIN-V"),
      models$formated_data[j,] |> mutate(type = "predicted", ARM = "Placebo")
    ) |> 
    mutate(
      type = type |> factor(levels = c("actual", "predicted")),
      ARM = ARM |> factor(levels = models$formated_data$ARM |> levels())
    )
  
  if (str_detect(models$m$family$family, "binomial")) {
    inv_link <- plogis
  } else if (models$m$family$family == "poisson") {
    inv_link <- exp
    stop("not implemented yet")
  } else{
    stop("not implemented yet")
  }
  
  # Before we can do the predictions, 
  # since we included more participants that those on which the model was fit, 
  # we check if the new data is in the domain
  
  domain <- 
    models$model_fit_data |> 
    select(starts_with(models$covariate)) |> 
    pivot_longer(cols = everything(), names_to = "covariate", values_to = "value") |>
    group_by(covariate) |>
    summarize(min = min(value), max = max(value))
  res <- 
    res |> 
    pivot_longer(cols = starts_with(models$covariate), names_to = "covariate", values_to = "value") |>
    left_join(domain, by = join_by(covariate)) |> 
    mutate(in_domain = (value >= min) & (value <= max)) |>
    select(-min, -max) |> 
    group_by(USUBJID) |> 
    mutate(in_domain = all(in_domain)) |> 
    ungroup() |> 
    pivot_wider(names_from = covariate, values_from = value) |> 
    mutate(in_domain_or_NA = ifelse(!in_domain, NA, 1))
  
  # If a participant is not in the domain, we will simply set their results to NA 
  # (see below)
  
  res <- 
    res |> 
    mutate(
      # predicted LINK & associated SE
      mu = predict(models$m, type = "link", new = res) * in_domain_or_NA, # we predict the link
      se_mu = predict(models$m, type = "link", new = res, se.fit = TRUE, level = conf.level)$se.fit * in_domain_or_NA,
      se_mu = ifelse(type == "actual", NA, se_mu), # we do not need the SE for the actual data
      # predicted probability of outcome (or actual outcome) & associated confidence interval
      p_hat = ifelse(type == "actual", !!sym(models$outcome), mu |> inv_link()) |> pmin(1), # y is the actual outcome for the actual data or the predicted probability. The pmin is necessary in case infinite values are returned.
      p_hat_CI_low = inv_link(mu - Z * se_mu) |> pmax(0), # confidence interval on the probability
      p_hat_CI_high = inv_link(mu + Z * se_mu) |> pmin(1)
    ) |> 
    select(-in_domain_or_NA) 
  
  # we now compute the difference in the LINK
  # if family = "binomial", this difference is the log(OR)
  # if family = "poisson", this difference is the log(RR)
  # we also compute the difference in probabilities
  res <- 
    res |> 
    arrange(USUBJID, type |> desc(), ARM |> desc()) |> 
    group_by(USUBJID) |> 
    mutate(delta_mu = mu[1] - mu[2], delta_p = p_hat[1] - p_hat[2]) |> 
    ungroup()
  
  ## Now, we compute the confidente intervals for the delta_mu
  
  me_res <- 
    marginaleffects::comparisons(
    models$m, 
    newdata = res |> filter(type == "actual", in_domain),
    comparison = "difference",
    type = "link",
    variable = "ARM",
    conf_level = conf.level
  ) 
  me_res_tb <-
    bind_cols(
      res |> filter(type == "actual", in_domain) |> extract(me_res$rowid, ) |> 
        select(USUBJID, ARM, models$outcome),
      tibble(
        delta_mu = me_res$estimate,
        delta_mu_CI_low = me_res$conf.low,
        delta_mu_CI_high = me_res$conf.high
      )
    ) 
  
  # check that we have the same results
  tmp_check <- res |> 
    filter(type == "actual", in_domain) |> 
    select(USUBJID, ARM, delta_mu) |> 
    left_join(me_res_tb, by = join_by(USUBJID, ARM)) |> 
    mutate(
      delta_mu_diff = delta_mu.x - delta_mu.y
    ) 
  if (!all(abs(tmp_check$delta_mu_diff) < 1e-10)) {
    stop("The delta_mu computed using marginaleffects is not the same as the one computed using the model.")
  }
  
  res <- 
    res |> 
    left_join(
      me_res_tb |> 
        select(USUBJID, delta_mu_CI_low, delta_mu_CI_high), 
      by = join_by(USUBJID)
    ) 
    
  list(long_res = res, me_res = me_res)
}

plot_counterfactuals <- function(counterfactuals, models = models, order_by = c("delta_mu"), add_topics = TRUE, facet_by = NULL){
  
  
  cdata <- 
    counterfactuals$long_res |> 
    filter(!is.na(delta_mu)) |> 
    mutate(type = type |> str_replace("actual", "Observed") |> str_to_title()) 
  
  # we order participants
  cdata <- 
    cdata |> 
    arrange(desc(type), ARM, !!sym(order_by), delta_mu) |> 
    mutate(USUBJID = USUBJID |> fct_inorder()) 
  
  models$original_data <- 
    models$original_data |> 
    mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels()))
  
  # if we need to facet by a variable, we add that variable to the datasets
  if (!is.null(facet_by)){
    if (!(facet_by %in% colnames(models$original_data))) stop("facet_by must be a column in models$original_data")
    models$original_data <- models$original_data |> mutate(facet = !!sym(facet_by))
    cdata <- 
      cdata |> 
        left_join(models$original_data |> select(USUBJID, facet), by = join_by(USUBJID))
       
  } else {
    models$original_data$facet <- "All"
    cdata$facet <- "All"
  }
  
  
  
  if (str_detect(models$m$family$family, "binomial")){
    ylabel <- "log(OR)"
  } else if (models$m$family$family == "poisson"){
    ylabel <- "log(RR)"
  } else{
    stop("not implemented yet")
  }
  g_delta <- 
    cdata |> 
    filter(type == "Observed") |> 
    select(USUBJID, ARM, delta_mu, delta_mu_CI_low, delta_mu_CI_high, facet) |>
    distinct() |>
    mutate(CI_includes_0 = sign(delta_mu_CI_low * delta_mu_CI_high) < 0) |>
    ggplot() + 
    aes(x = USUBJID, y = delta_mu, color = ARM) +
    geom_hline(yintercept = 0, col = "black") + 
    geom_linerange(
      aes(ymin = delta_mu_CI_low, ymax = delta_mu_CI_high), 
      linewidth = 0.2, alpha = 0.75
      ) +
    geom_point(aes(shape = CI_includes_0), alpha = 0.7) + 
    xlab("Participants") + 
    ylab(str_c("\n",ylabel)) +
    scale_shape_manual(
      "Sign.", values = c(1, 16), breaks = c(TRUE, FALSE), labels = c("CI include 0", "CI does not includes 0"),
      guide = guide_legend(direction = "vertical")
      ) +
    scale_color_manual(
      "Arm", values = get_arm_colors(), 
      guide = guide_legend(direction = "vertical")
      ) +
    theme(
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "right"
      )
  
  
  g_prob <- 
    cdata |> 
    mutate(arm = ARM) |> 
    group_by(USUBJID) |>
    mutate(ARM = ARM[type == "Observed"]) |>
    ggplot() +
    aes(x = USUBJID, y = p_hat, color = arm, shape = type) + 
    geom_linerange(
      aes(ymin = p_hat_CI_low, ymax = p_hat_CI_high), 
      linewidth = 0.2, alpha = 0.75
    )  +
    geom_point() +
    xlab("Participants") + 
    ylab("(Pred. prob. of)\noutcome") +
    scale_color_manual(
      "Arm", values = get_arm_colors(), 
      guide = "none" # guide_legend(direction = "vertical")
    ) +
    scale_shape_manual(
      "Data", values = c(4, 16), 
      guide = guide_legend(direction = "vertical")
      ) +
    theme(
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "left"
    )
  
  g_V <- 
    models$original_data |> 
    filter(USUBJID %in% cdata$USUBJID) |>
    mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels())) |> 
    select(USUBJID, ARM, !!sym(models$covariate), facet) |>
    unnest(cols = !!sym(models$covariate), names_sep = "_") |>
    pivot_longer(cols = -c(USUBJID, ARM, facet), names_to = "variable", values_to = "value") |> 
    mutate(variable = variable  |>str_remove_all(str_c(models$covariate, "_"))) |>
    ggplot() +
    aes(x = USUBJID, y = value, fill = variable) +
    geom_col()  +
    facet_grid(variable ~ ., scales = "free_y") +
    scale_fill_discrete(
      models$covariate |> str_replace_all("_", " "), 
      guide = guide_legend(direction = "vertical")
    ) +
    theme(
      strip.text.y = element_blank(),
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "left"
    ) +
    xlab("Participants") +
    ylab("\nValue")
  
  
  if (add_topics) {
    g_sub <- 
      models$original_data |> 
      select(USUBJID, ARM, topic, facet) |> 
      filter(USUBJID %in% cdata$USUBJID) |>
      mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels())) |>
      unnest(cols = topic, names_sep = "_") |>
      pivot_longer(cols = -c(USUBJID, ARM, facet), names_to = "topic", values_to = "rel_ab") |> 
      mutate(topic = topic  |> str_remove_all("topic_") |> fct_inorder()) |> 
      ggplot() +
      aes(x = USUBJID, y = rel_ab, fill = topic) +
      geom_col() +
      scale_fill_manual(
        "pre-MTZ topic", values = get_topic_colors(colnames(models$original_data$topic)),
        guide = guide_legend(direction = "vertical", ncol = 2)
        ) +
      xlab("Participants") +
      ylab("\nRel. abundance") +
      theme(
        axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
        legend.position = "right", legend.justification = "left"
        ) 
  }
  
  if (!is.null(facet_by)) {
    g_delta <- g_delta + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    g_prob <- g_prob + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    g_V <- g_V + facet_grid(variable ~ facet, scales = "free", space = "free_x")
    if (add_topics) {
      g_sub <- g_sub + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    }
  }
  
  if (add_topics) {
    patchwork <-  g_delta + g_prob + g_V + g_sub
  } else {
    patchwork <-  g_delta + g_prob + g_V 
  }
  
  patchwork <- patchwork & theme(legend.text = element_text(hjust = 0), legend.box.just = "left")

  patchwork +
    plot_layout(ncol = 1) +
    plot_annotation(
      title = 
        str_c(
          "Counterfactuals for ", 
          models$outcome |> 
            str_replace("rBV_week", "rBV_by_week") |> 
            str_replace("Lc_week", "Lc_at_week") |> 
            str_replace_all("_", " ") 
              ) 
    )
  
}


plot_counterfactuals_v1 <- function(counterfactuals, models = models, order_by = c("delta_mu"), add_subcommunities = TRUE, facet_by = NULL){
  
  
  cdata <- 
    counterfactuals |> 
    filter(!is.na(delta_mu)) |> 
    mutate(type = type |> str_replace("actual", "Observed") |> str_to_title()) 
  
  # we order participants
  cdata <- 
    cdata |> 
    arrange(type, !!sym(order_by), delta_mu) |> 
    mutate(USUBJID = USUBJID |> fct_inorder()) 
  
  models$original_data <- 
    models$original_data |> 
    mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels()))
  
  # if we need to facet by a variable, we add that variable to the datasets
  if (!is.null(facet_by)){
    if (!(facet_by %in% colnames(models$original_data))) stop("facet_by must be a column in models$original_data")
    models$original_data <- models$original_data |> mutate(facet = !!sym(facet_by))
    cdata <- 
      cdata |> 
      left_join(models$original_data |> select(USUBJID, facet), by = join_by(USUBJID))
    
  } else {
    models$original_data$facet <- "All"
    cdata$facet <- "All"
  }
  
  
  
  if (str_detect(models$m$family$family, "binomial")){
    ylabel <- "log(OR)"
  } else if (models$m$family$family == "poisson"){
    ylabel <- "log(RR)"
  } else{
    stop("not implemented yet")
  }
  g_delta <- 
    cdata |> 
    filter(type == "Observed") |> 
    select(USUBJID, ARM, delta_mu, delta_mu_CI_low, delta_mu_CI_high, facet) |>
    distinct() |>
    mutate(CI_includes_0 = sign(delta_mu_CI_low * delta_mu_CI_high) < 0) |>
    ggplot() + 
    aes(x = USUBJID, y = delta_mu, color = ARM) +
    geom_hline(yintercept = 0, col = "black") + 
    geom_linerange(
      aes(ymin = delta_mu_CI_low, ymax = delta_mu_CI_high), 
      width = 0.1, alpha = 0.5
    ) +
    geom_point(aes(shape = CI_includes_0), alpha = 0.7) + 
    xlab("Participants") + 
    ylab(str_c("\n",ylabel)) +
    scale_shape_manual(
      "Sign.", values = c(1, 16), breaks = c(TRUE, FALSE), labels = c("CI include 0", "CI does not includes 0"),
      guide = guide_legend(direction = "vertical")
    ) +
    scale_color_manual(
      "Arm", values = get_arm_colors(), 
      guide = guide_legend(direction = "vertical")
    ) +
    theme(
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "right"
    )
  
  
  g_prob <- 
    cdata |> 
    mutate(arm = ARM) |> 
    group_by(USUBJID) |>
    mutate(ARM = ARM[type == "Observed"]) |>
    ggplot() +
    aes(x = USUBJID, y = p_hat, color = arm, shape = type) + 
    geom_linerange(
      aes(ymin = p_hat_CI_low, ymax = p_hat_CI_high), 
      width = 0.1, alpha = 0.5
    )  +
    geom_point() +
    xlab("Participants") + 
    ylab("(Pred. prob. of)\noutcome") +
    scale_color_manual(
      "Arm", values = get_arm_colors(), 
      guide = "none" # guide_legend(direction = "vertical")
    ) +
    scale_shape_manual(
      "Data", values = c(4, 16), 
      guide = guide_legend(direction = "vertical")
    ) +
    theme(
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "left"
    )
  
  g_V <- 
    models$original_data |> 
    filter(USUBJID %in% cdata$USUBJID) |>
    mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels())) |> 
    select(USUBJID, ARM, !!sym(models$covariate), facet) |>
    unnest(cols = !!sym(models$covariate), names_sep = "_") |>
    pivot_longer(cols = -c(USUBJID, ARM, facet), names_to = "variable", values_to = "value") |> 
    mutate(variable = variable  |>str_remove_all(str_c(models$covariate, "_"))) |>
    ggplot() +
    aes(x = USUBJID, y = value, fill = variable) +
    geom_col()  +
    facet_grid(variable ~ ., scales = "free_y") +
    scale_fill_discrete(
      models$covariate |> str_replace_all("_", " "), 
      guide = guide_legend(direction = "vertical")
    ) +
    theme(
      strip.text.y = element_blank(),
      axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
      legend.position = "right", legend.justification = "left"
    ) +
    xlab("Participants") +
    ylab("\nValue")
  
  
  if (add_subcommunities) {
    g_sub <- 
      models$original_data |> 
      select(USUBJID, ARM, subcommunity, facet) |> 
      filter(USUBJID %in% cdata$USUBJID) |>
      mutate(USUBJID = USUBJID |> factor(levels = cdata$USUBJID |> levels())) |>
      unnest(cols = subcommunity, names_sep = "_") |>
      pivot_longer(cols = -c(USUBJID, ARM, facet), names_to = "subcommunity", values_to = "rel_ab") |> 
      mutate(subcommunity = subcommunity  |> str_remove_all("subcommunity_") |> fct_inorder()) |> 
      ggplot() +
      aes(x = USUBJID, y = rel_ab, fill = subcommunity) +
      geom_col() +
      scale_fill_manual(
        "pre-MTZ subcommunity", values = get_topic_colors(colnames(models$original_data$subcommunity)),
        guide = guide_legend(direction = "vertical", ncol = 2)
      ) +
      xlab("Participants") +
      ylab("\nRel. abundance") +
      theme(
        axis.text.x = element_blank(), # element_text(angle = 90, hjust = 1),
        legend.position = "right", legend.justification = "left"
      ) 
  }
  
  if (!is.null(facet_by)) {
    g_delta <- g_delta + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    g_prob <- g_prob + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    g_V <- g_V + facet_grid(variable ~ facet, scales = "free", space = "free_x")
    if (add_subcommunities) {
      g_sub <- g_sub + facet_grid(. ~ facet, scales = "free_x", space = "free_x")
    }
  }
  
  if (add_subcommunities) {
    patchwork <-  g_delta + g_prob + g_V + g_sub
  } else {
    patchwork <-  g_delta + g_prob + g_V 
  }
  
  patchwork <- patchwork & theme(legend.text = element_text(hjust = 0), legend.box.just = "left")
  
  patchwork +
    plot_layout(ncol = 1) +
    plot_annotation(
      title = 
        str_c(
          "Counterfactuals for ", 
          models$outcome |> 
            str_replace("rBV_week", "rBV_by_week") |> 
            str_replace("Lc_week", "Lc_at_week") |> 
            str_replace_all("_", " ") 
        ) 
    )
  
}
