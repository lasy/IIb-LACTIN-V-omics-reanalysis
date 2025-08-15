
plot_longitudinal_cytokines <- function(cytokine_long, value_label = NULL){
  if (is.null(value_label)) value_label <- "transformed concentration"
  cytokine_long |> 
    ggplot(aes(x = Visit, y = value, col = ARM)) +
    geom_line(aes(group = USUBJID), alpha = 0.3) +
    geom_point(alpha = 0.3, size = 0.25) +
    facet_grid(ARM ~ cytokine) +
    scale_color_manual("Intervention arm", values = get_arm_colors()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Visit") +
    ylab(value_label) +
    guides(col = "none")
}


plot_longitudinal_cytokines_by_arm_and_category <- function(cytokine_long, value_label = NULL){
  if (is.null(value_label)) value_label <- "transformed concentration"
  if (str_detect(value_label, "PC1")) col_ref <- "black" else col_ref <- "transparent"
  cytokine_long |>
    filter(PIPV) |> 
    ggplot(aes(x = ARM, y = value)) +
    geom_hline(yintercept = 0, col = col_ref, linewidth = 1) +
    geom_boxplot(outlier.shape = NA, col = "gray50", fill = "gray80") +
    geom_jitter(aes(col = microbiota), size = 0.75, height = 0, width = 0.15) +
    scale_color_manual("Microbiota cut-off", values = get_topic_colors(c("I","III","IV"))) +
    facet_grid(cytokine ~ Visit) +
    xlab("Intervention arm") + 
    ylab(value_label) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}


plot_longitudinal_cytokines_by_category <- function(cytokine_long, value_label = NULL){
  if (is.null(value_label)) value_label <- "transformed concentration"
  cytokine_long |>
    filter(PIPV) |> 
    ggplot(aes(x = microbiota, y = value)) +
    geom_hline(yintercept = 0, col = "black", linewidth = 1) +
    geom_boxplot(outlier.shape = NA, col = "gray50", fill = "gray80") +
    geom_jitter(aes(col = microbiota), size = 0.75, height = 0, width = 0.15) +
    scale_color_manual("Microbiota cut-off", values = get_topic_colors(c("I","III","IV"))) +
    facet_grid(cytokine ~ Visit) +
    xlab("Intervention arm") + 
    ylab(value_label) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}



plot_longitudinal_cytokines_by_ctv05_cat <- function(cytokine_long, value_label = NULL){
  if (is.null(value_label)) value_label <- "transformed concentration"
  cytokine_long |>
    filter(PIPV) |> 
    ggplot(aes(x = microbiota_ctv05, y = value)) +
    geom_hline(yintercept = 0, col = "black", linewidth = 1) +
    geom_boxplot(outlier.shape = NA, col = "gray50", fill = "gray80") +
    geom_jitter(aes(col = microbiota_ctv05), size = 0.75, height = 0, width = 0.15) +
    scale_color_manual(
      "Microbiota category", 
      breaks = cytokine_long$microbiota_ctv05 |> levels(),
      values = c(get_topic_colors(c("I.a", "I.b","III","IV")), "gray30"),
      guide = guide_legend(override.aes = list(size = 2))
    ) +
    facet_grid(cytokine ~ Visit) +
    xlab("") + 
    ylab(value_label) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}



plot_cytokine_to_preMTZ <- function(cytokines_to_preMTZ, value_label = NULL, add_post_MTZ_med = TRUE){
  if (is.null(value_label)) value_label <- "transformed concentration"
  if (add_post_MTZ_med) {
    post_MTZ_med <- 
      cytokines_to_preMTZ |> 
      filter(Visit == "Post-MTZ") |> 
      group_by(cytokine) |> 
      summarise(post_MTZ_med = median(diff, na.rm = TRUE), .groups = "drop")
  }
  value_label <- str_c("Difference in ", value_label, "\nfrom pre-MTZ visit")
  
  g <- 
    cytokines_to_preMTZ |> 
    filter(!is.na(microbiota)) |> 
    ggplot() + 
    aes(x = ARM, y = diff) +
    geom_hline(yintercept = 0, col = "black", linewidth = 1) 
  if (add_post_MTZ_med) {
    g <- 
      g +
      geom_hline(
        data = post_MTZ_med, aes(yintercept = post_MTZ_med), 
        col = "red3", linewidth = 0.5, linetype = "31"
        )
  }
  
  g +
    geom_boxplot(outlier.shape = NA, col = "gray50", fill = "gray80") +
    geom_jitter(aes(col = microbiota), size = 0.75, height = 0, width = 0.15) +
    scale_color_manual(
      "Microbiota category", values = get_topic_colors(c("I","III","IV")),
      guide = guide_legend(override.aes = list(size = 2))
      
      ) +
    facet_grid(cytokine ~ Visit) +
    xlab("Intervention arm") + 
    ylab(value_label) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}


effects_of_intervention <-
  function(cytokine, cytokines_long, use_ranks = FALSE){
    tmp <- 
      cytokines_long |> 
      filter(cytokine == !!cytokine, PIPV) |> 
      select(USUBJID, ARM, value, Visit) |> 
      mutate(ARM = factor(ARM, levels = c("Placebo", "LACTIN-V")))
    
    if (use_ranks) {
      tmp$value <- rank(tmp$value, ties.method = "random") 
    }
    
    # fit the lmm
    mmod <- lme4::lmer(value ~ ARM * Visit + (1|USUBJID), data = tmp) 
    # check if model assumptions are met
    sim_resid <- DHARMa::simulateResiduals(mmod)
    t_u <- DHARMa::testUniformity(sim_resid, plot = FALSE)
    t_d <- DHARMa::testDispersion(sim_resid, plot = FALSE)
    t_o <- DHARMa::testOutliers(sim_resid, plot = FALSE)
    
    # check the model results
    # mmod |> summary() 
    # car::Anova(mmod, type = "III")
    
    # contrasts between the two arms
    
    # emm <- emmeans::emmeans(mmod, ~ ARM, at = list(Visit = "Week 24"))  
    # pairs(emm)
    
    # emm <- emmeans::emmeans(mmod, ~ ARM * Visit, at = list(Visit = "Week 24"))  
    # pairs(emm)
    
    factor <- ifelse(use_ranks, 1 / nrow(tmp) * 100, 1)
    
    emm <- emmeans::emmeans(mmod, ~ ARM | Visit)
    pairs_res <- pairs(emm, simple = "ARM", by = "Visit")
    # pairs_res_summ <- pairs_res |> summary()
    ci_results <- confint(pairs_res)
    tibble(
      cytokine = cytokine,
      t_u_pval = t_u$p.value,
      t_d_pval = t_d$p.value,
      t_o_pval = t_o$p.value,
      visit = ci_results$Visit,
      estimate = ci_results$estimate * factor,
      CI_lower = ci_results$lower.CL * factor,
      CI_upper = ci_results$upper.CL * factor,
    )
  }


effects_of_intervention_wilcox <- function(cytokine, cytokines_to_preMTZ) {
  tmp <- 
    cytokines_to_preMTZ |> 
    filter(cytokine == !!cytokine, Visit == "Week 24") |> 
    select(USUBJID, ARM, diff, Visit) |> 
    mutate(ARM = factor(ARM, levels = c("LACTIN-V", "Placebo")))
  
  # Wilcoxon test for differences in cytokine concentrations between the two arms at week 24
  
  wilcox_res <- wilcox.test(diff ~ ARM, data = tmp, conf.int = TRUE)
  tibble(
    cytokine = cytokine,
    W = wilcox_res$statistic,
    p_value = wilcox_res$p.value,
    estimate = wilcox_res$estimate,
    CI_lower = wilcox_res$conf.int[1],
    CI_upper = wilcox_res$conf.int[2]
  )
  
}

