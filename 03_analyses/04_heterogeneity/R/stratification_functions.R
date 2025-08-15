
get_summaries <- function(data, outcome, covariate) {
  data |> 
    filter(!is.na(.data[[outcome]]), !is.na(.data[[covariate]]), !is.na(ARM)) |> 
    group_by(ARM, .data[[covariate]]) |> 
    summarize(
      r = mean(.data[[outcome]], na.rm = TRUE),
      pos = sum(.data[[outcome]] > 0, na.rm = TRUE),
      n = n(),
      CI = scoreci(x = pos, n = n, conf.level = 0.95)$conf.int |> t(),
      .groups = "drop"
    )
}

get_RR <- function(data, outcome, covariate, min_n_each_arm){
  data |> 
    select(ARM, all_of(c(outcome, covariate))) |>
    drop_na() |> 
    group_by(ARM, .data[[covariate]]) |> mutate(min_n = min(n())) |> ungroup() |> 
    group_by(.data[[covariate]]) |> mutate(min_n = min(min_n)) |> ungroup() |> 
    filter(min_n >= min_n_each_arm) |> 
    group_by(.data[[covariate]])  |> 
    summarize(
      res = 
        epitools::riskratio(x = ARM, y = .data[[outcome]], method = "wald")$measure[2,] |>
        t()
    ) |> 
    mutate(
      estimate = res[,1],
      CI_lo = res[,2],
      CI_hi = res[,3]
    ) |> 
    select(-res)
  
}

get_OR <- function(data, outcome, covariate, min_n_each_arm){
  data |> 
    select(ARM, all_of(c(outcome, covariate))) |>
    drop_na() |> 
    group_by(ARM, .data[[covariate]]) |> mutate(min_n = min(n())) |> ungroup() |> 
    group_by(.data[[covariate]]) |> mutate(min_n = min(min_n)) |> ungroup() |> 
    filter(min_n >= min_n_each_arm) |> 
    group_by(.data[[covariate]])  |> 
    summarize(
      res = 
        epitools::oddsratio(x = ARM, y = .data[[outcome]], method = "wald")$measure[2,] |>
        t()
    ) |> 
    mutate(
      estimate = res[,1],
      CI_lo = res[,2],
      CI_hi = res[,3]
    ) |> 
    select(-res)
  
}


compute_prob_differences <- function(x, y, conf.level = 0.95){
  
  
  # cat("here\n")
  
  tmp <- table(y, x) 
  n_0 <- sum(tmp[1,])
  n_1 <- sum(tmp[2,])
  x_0 <- tmp[1,2]
  x_1 <- tmp[2,2]
  p_0 <- tmp[1, 2] / n_0
  p_1 <- tmp[2, 2] / n_1
  
  # cat("p_0": p_0, "\n")
  # cat("p_1: ", p_1, "\n")
  
  RD <- p_1 - p_0
  
  # Wilson's method for CI
  
  # cat("x1": x_1, "\n")
  # cat("m1: ", n_1, "\n")
  # cat("x2: ", x_0, "\n")
  # cat("m2: ", 1, "\n")
  # cat("n2: ", n_0, "\n")
  
  res <- 
    binGroup2::propDiffCI(
      x1 = x_1, m1 = 1, n1 = n_1, x2 = x_0, m2 = 1, n2 = n_0, 
      ci.method = "score"
      )
  
  
  # Z <- qnorm(1 - (1 - conf.level)/2)
  # se_0 <- sqrt(p_0 * (1 - p_0) / n_0)
  # se_0 <- sqrt((p_0 * (1 - p_0) + (Z^2)/(4 * n_0))/n_0)/(1 + (Z^2)/n_0)
  # se_1 <- sqrt(p_1 * (1 - p_1) / n_1)
  # se_1 <- sqrt((p_1 * (1 - p_1) + (Z^2)/(4 * n_1))/n_1)/(1 + (Z^2)/n_1)
  # RD_se <- sqrt(se_0^2 + se_1^2)
  # RD_CI_lo <- RD - Z * RD_se
  # RD_CI_hi <- RD + Z * RD_se
  
  c(RD = RD, RD_CI_lo = res$lcl, RD_CI_hi = res$ucl)
}

get_RD <- function(data, outcome, covariate, min_n_each_arm){
  data |> 
    select(ARM, all_of(c(outcome, covariate))) |>
    drop_na() |> 
    group_by(ARM, .data[[covariate]]) |> mutate(min_n = min(n())) |> ungroup() |> 
    group_by(.data[[covariate]]) |> 
    mutate(min_n = min(min_n), n_arm = ARM |> unique() |> length()) |> 
    ungroup() |> 
    filter(n_arm == 2, min_n >= min_n_each_arm) |> 
    group_by(.data[[covariate]])  |> 
    summarize(
      res = compute_prob_differences(x = .data[[outcome]], y = ARM) |> t()
    ) |> 
    mutate(
      estimate = res[,1],
      CI_lo = res[,2],
      CI_hi = res[,3]
    ) |> 
    select(-res)
  
}


plot_stratified_risks <- function(AYV, outcomes, strata, min_n_in_each_arm = 5){
  
  AYV_summary <- 
    map(
      outcomes, 
      ~ get_summaries(AYV, outcome = .x, strata) |> mutate(outcome = .x)
    ) |> 
    bind_rows()
  
  # prettify labels
  AYV_summary <- 
    AYV_summary |> 
    mutate(
      outcome_label = 
        outcome |> 
        str_replace("Lc_", "≥50% L. crisp. at ") |> 
        str_replace("rBV_", "any rBV by ") |> 
        str_replace("week_", "week ") 
    )
  
  # filter strata that have at least `min_n_in_each_arm` participants in each arm
  AYV_summary <- 
    AYV_summary |> 
    group_by(.data[[strata]], outcome) |> 
    mutate(smallest_n_in_each_arm = min(n), n_arm = ARM |> n_distinct()) |> 
    ungroup() |> 
    filter(n_arm >= 2, smallest_n_in_each_arm >= min_n_in_each_arm)
  
  
  # plot the observed rates (estimated probabilities + CI)
  g_r <- 
    AYV_summary |> 
    arrange(.data[[strata]]) |> 
    mutate(
      cov = .data[[strata]] |> str_wrap(15) |> fct_inorder(),
      x = as.numeric(cov) + (as.numeric(ARM) - 1.5)/4
    ) |> 
    ggplot() +
    aes(x = cov, y = r, color = ARM) +
    facet_grid(. ~ outcome_label) +
    geom_hline(yintercept = c(0, 1), col = "gray") +
    geom_errorbar(aes(ymin = CI[,1], ymax = CI[,2]), width = 0.2, position = position_dodge(width = 0.4)) +
    geom_point(size = 5, position = position_dodge(width = 0.4)) +
    geom_text(aes(label = n, group = ARM), col = "black", size = 3, position = position_dodge(width = 0.4)) +
    scale_color_manual("Arm", values = get_arm_colors(), guide = guide_legend(direction  = "vertical")) +
    guides(size = "none", shape = "none") +
    xlab("") + ylab("Rates") +
    # labs(
    #   caption = "95% CI for the rates are computed using Wilson's formula."
    # ) +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0),
      legend.position = "right"
    )
  g_r
}


plot_stratified_ratios <- function(AYV, outcomes, strata, min_n_in_each_arm = 5){
  
  ratios <- 
    map(
      outcomes, 
      ~ get_RR(AYV, outcome = .x, covariate = strata, min_n_each_arm = min_n_in_each_arm) |> 
        mutate(outcome = .x)
    ) |> 
    bind_rows() 
  
  # prettify labels
  ratios <- 
    ratios |> 
    mutate(
      outcome_label = 
        outcome |> 
        str_replace("Lc_", "≥50% L. cris. at ") |> 
        str_replace("rBV_", "any rBV by ") |> 
        str_replace("week_", "week ") 
    )
  
  # benefit or risk?
  ratios <- 
    ratios |> 
    mutate(benefit = ifelse(str_detect(outcome, "Lc"), 1, -1) * (estimate - 1))
  
  g_rr <- 
    ratios |> 
    arrange(.data[[strata]]) |> 
    mutate(cov = .data[[strata]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot() +
    aes(x = cov, y = estimate, color = benefit) +
    facet_grid(. ~ outcome_label) +
    geom_hline(yintercept = 1, col = "gray") +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi), width = 0.2) +
    geom_point(size = 3) +
    xlab("") + ylab("Benefit/Risk ratios") +
    scale_color_gradient2(
      midpoint = 1, low = "tomato", high = "dodgerblue2", mid = "black",
      guide = guide_legend(direction = "vertical")
      )
  
  g_rr
}


plot_stratified_differences <- function(AYV, outcomes, strata, min_n_in_each_arm = 5){
  
  
  differences <- 
    map(
      outcomes, 
      ~ get_RD(AYV, outcome = .x, covariate = strata, min_n_each_arm = min_n_in_each_arm) |> 
        mutate(outcome = .x)
    ) |> 
    bind_rows() 
  
  
  
  # prettify labels
  differences <- 
    differences |> 
    mutate(
      outcome_label = 
        outcome |> 
        str_replace("Lc_", "≥50% L. crisp. at ") |> 
        str_replace("rBV_", "any rBV by ") |> 
        str_replace("week_", "week ") 
    )
  
  # benefit or risk?
  differences <- 
    differences |> 
    mutate(benefit = ifelse(str_detect(outcome, "Lc"), 1, -1) * estimate)
  
  g_rd <- 
    differences |> 
    arrange(.data[[strata]]) |> 
    mutate(cov = .data[[strata]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot() +
    aes(x = cov, y = estimate, color = benefit) +
    facet_grid(. ~ outcome_label) +
    geom_hline(yintercept = 0, col = "gray") +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi), width = 0.2) +
    geom_point(size = 3) +
    xlab("") + ylab("Rate differences") +
    scale_color_gradient2(
      "Benefit\n(± rate diff.)", 
      limits = c(-1, 1), breaks = -1:1,
      midpoint = 0, low = "tomato", high = "dodgerblue2", mid = "gray60",
      guide = guide_colorbar(direction = "vertical")
      ) +
    # labs(
    #   caption = "95%CI computed using Wilson score method for differences."
    # ) +
    theme(
      legend.position = "right"
    )
  
  g_rd
}




plot_risks <- function(AYV, outcome = "Lc_week_12", covariate = "Race", min_n_each_arm = 5) {
  
  AYV_summary <- 
    get_summaries(AYV, outcome, covariate) 
  
  if (str_detect(outcome, "Lc")) {
    outcome_label <- "successful L. crispatus colonization at "
  } else {
    outcome_label <- "any rBV by "
  }
  
  if (str_detect(outcome, "week_12")) {
    outcome_label <- str_c(outcome_label, "week 12")
  } else {
    outcome_label <- str_c(outcome_label, "week 24")
  }
  
  # plots of the observed rates (estimated probabilities + CI)
  g_r <- 
    AYV_summary |> 
    arrange(.data[[covariate]]) |> 
    mutate(cov = .data[[covariate]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot(aes(x = r, y = ARM, color = ARM)) +
    geom_errorbar(aes(xmin = CI[,1], xmax = CI[,2])) +
    geom_point(aes(shape = ARM), size = 3) +
    geom_text(aes(label = n), col = "black", size = 3) +
    scale_color_manual(values = get_arm_colors()) +
    facet_grid(cov ~ .) +
    guides(size = "none", shape = "none", color = "none") +
    ylab("") + xlab(str_c("proportion of ", outcome_label) |> str_wrap(45)) +
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) 
  
  if (any(AYV_summary$n < min_n_each_arm)) {
    
    g_r_2 <- 
      AYV_summary |> 
      arrange(.data[[covariate]]) |> 
      mutate(cov = .data[[covariate]] |> str_wrap(15) |> fct_inorder()) |> 
      filter(n > min_n_each_arm) |> 
      group_by(cov) |> 
      mutate(has_both_arms = (n() == 2)) |> 
      ungroup() |> 
      filter(has_both_arms) |>
      ggplot(aes(x = r, y = ARM, color = ARM)) +
      geom_errorbar(aes(xmin = CI[,1], xmax = CI[,2])) +
      geom_point(aes(shape = ARM), size = 3) +
      geom_text(aes(label = n), col = "black", size = 3) +
      scale_color_manual(values = get_arm_colors()) +
      facet_grid(cov ~ .) +
      guides(size = "none", shape = "none", color = "none") +
      ylab("") + xlab(str_c("proportion of ", outcome_label) |> str_wrap(45)) +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) 
    
  } else {g_r_2 <- NULL}
  
  AYV_RD <- get_RD(AYV, outcome, covariate, min_n_each_arm)
  
  g_rd <- 
    AYV_RD |> 
    arrange(.data[[covariate]]) |> 
    mutate(cov = .data[[covariate]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot(aes(x = estimate, y = "")) +
    geom_vline(xintercept = 0, col = "gray") +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi)) +
    geom_point(size = 3) +
    facet_grid(cov ~ ., scales = "free") +
    ylab("") + xlab("differences") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
  
  
  AYV_RR <- get_RR(AYV, outcome, covariate, min_n_each_arm)
  
  g_rr <- 
    AYV_RR |> 
    arrange(.data[[covariate]]) |> 
    mutate(cov = .data[[covariate]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot(aes(x = estimate, y = "")) +
    geom_vline(xintercept = 1, col = "gray") +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi)) +
    geom_point(size = 3) +
    facet_grid(cov ~ ., scales = "free") +
    ylab("") + xlab("ratios") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
  
  AYV_OR <- get_OR(AYV, outcome, covariate, min_n_each_arm)
  
  g_or <- 
    AYV_OR |> 
    arrange(.data[[covariate]]) |> 
    mutate(cov = .data[[covariate]] |> str_wrap(15) |> fct_inorder()) |> 
    ggplot(aes(x = estimate, y = "")) +
    geom_vline(xintercept = 1, col = "gray") +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi)) +
    geom_point(size = 3) +
    facet_grid(cov ~ ., scales = "free") +
    ylab("") + xlab("odds ratios") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
  
  
  if (is.null(g_r_2)) {
    g_r + g_rd + g_rr + g_or + plot_layout(nrow = 1)
  } else {
    g_r + g_r_2 + g_rd + g_rr  + g_or + plot_layout(nrow = 1)
  }
}
