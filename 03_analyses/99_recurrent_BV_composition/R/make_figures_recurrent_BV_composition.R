
make_distribution_figures <- function(df, var, target_visit){
  if (!var %in% colnames(df)) {
    stop(str_c("Variable ", var, " not found in the data frame."))
  }
  
  if (var == "BC_pre_MTZ_target") {
    label <- 
      str_c(
        "Bray-Curtis dissimilarity\nbetween ASV rel. ab. at the\npre-MTZ and the ",
        get_visit_labels(target_visit) |> str_to_lower()," visit"
      )
    var_limits <- c(0, 1)
  } else if (var == "percentile_rank") {
    label <- 
      str_c(
        "Percentile rank\nof BC dissimilarity to\nown pre-MTZ microbiota"
      )
    var_limits <- c(0, 100)
  } else{
    label <- var
  }
  
  df$var <- df[[var]]
  
  BV_diagnosis_colors <- c("Yes" = "red", "No" = "steelblue1")
  
  g_by_arm_and_BV <-  
    df |> 
    filter(!is.na(BV)) |> 
    ggplot() +
    aes(y = var, x = BV, col = BV, fill = BV) +
    facet_grid(. ~ Arm) +
    geom_violin(alpha = 0.5, col = "transparent") + # , col = "white", draw_quantiles = 0.5
    geom_point(
      data = df |> filter(!is.na(BV)) |> group_by(BV, Arm) |> summarize(median = median(var), .groups = "drop"),
      aes(y = median),
      col = "white", size = 4
      ) +
    ggbeeswarm::geom_quasirandom() +
    scale_color_manual(values = BV_diagnosis_colors) +
    scale_fill_manual(values = BV_diagnosis_colors) +
    ylab(label) +
    expand_limits(y = var_limits)
  
  patch <- 
    
    
    df |> 
    ggplot() +
    aes(x = var) +
    facet_grid(Arm ~ . , scales = "free_y") +
    geom_histogram(bins = 30) +
    xlab(label |> str_replace_all("\n"," ")) +
    expand_limits(x = var_limits) +
    
    df |> 
    ggplot() +
    aes(y = var, x = Arm, col = Arm, fill = Arm) +
    geom_violin(alpha = 0.5, col = "transparent") +
    geom_point(
      data = df |> filter(!is.na(BV)) |> group_by(Arm) |> summarize(median = median(var), .groups = "drop"),
      aes(y = median),
      col = "white", size = 4
    ) +
    ggbeeswarm::geom_quasirandom() +
    scale_color_manual(values = get_arm_colors()) +
    scale_fill_manual(values = get_arm_colors()) +
    ylab(label) +
    expand_limits(y = var_limits) +
    
    df |> 
    filter(!is.na(BV)) |> 
    ggplot() +
    aes(y = var, x = BV, col = BV, fill = BV) +
    geom_violin(alpha = 0.5, col = "transparent") +
    geom_point(
      data = df |> filter(!is.na(BV)) |> group_by(BV) |> summarize(median = median(var), .groups = "drop"),
      aes(y = median),
      col = "white", size = 4
    ) +
    ggbeeswarm::geom_quasirandom() +
    scale_color_manual(values = BV_diagnosis_colors) +
    scale_fill_manual(values = BV_diagnosis_colors) +
    ylab(label) +
    expand_limits(y = var_limits) +
    
    g_by_arm_and_BV +
    
    plot_layout(
      design = 
        "
      AAA
      BCD
    "
    ) &
    theme(legend.position = "none")
  
  list(
    patch = patch,
    g_by_arm_and_BV = g_by_arm_and_BV,
    label = label,
    var_limits = var_limits
  )
  
}


BC_and_PR_each_participant <- function(BC_all_others, df, target_visit) {
  
  colors_same_pid <-  c("Same participant" = "red", "Other participants" = "gray75")
  pid_breaks <- 
    BC_all_others |> 
    filter(has_own_BC) |> 
    select(USUBJID, pid_label) |> 
    distinct() |> 
    arrange(USUBJID) 
  axis_text_y_size <- 7
  
  BC_all_others |> 
    filter(has_own_BC) |>
    left_join(
      df |> 
        select(USUBJID, BV) |> 
        mutate(USUBJID = USUBJID |> factor(levels = BC_all_others$USUBJID |> levels())), 
      by = "USUBJID"
    ) |> 
    arrange(same_USUBJID) |> 
    ggplot() +
    aes(x = BC, y = USUBJID |> fct_rev(), fill = same_USUBJID_label) +
    geom_violin(scale = "width", alpha = 0.5, col = "transparent", drop = TRUE) +
    geom_point(aes(size = same_USUBJID_label, col = same_USUBJID_label, shape = BV)) +
    scale_size_manual("", values = c(2, 0.5)) +
    scale_shape_manual("", values = c(16, 1), labels = c("BV", "no BV")) +
    scale_color_manual("", values = colors_same_pid) +
    scale_fill_manual("", values = colors_same_pid) +
    guides(fill = "none") +
    scale_y_discrete(
      "Participants, ordered by the similarity to their pre-MTZ microbiota",
      breaks = pid_breaks$USUBJID,
      labels = pid_breaks$pid_label
    ) +
    xlab(
      str_c("BC dissimilarity between ASV rel. ab\n at the ", 
            target_visit |> get_visit_labels() |> str_to_lower(),
            " and pre-MTZ visits")
      ) +
    theme(
      axis.text.y = element_text(size = axis_text_y_size)
    ) +
    
    BC_all_others |> 
    filter(same_USUBJID) |>
    left_join(
      df |> select(USUBJID, Arm, BV) |> mutate(USUBJID = USUBJID |> factor(levels = BC_all_others$USUBJID |> levels())), 
      by = "USUBJID"
    ) |>
    ggplot() +
    aes(x = percentile_rank, y = USUBJID |> fct_rev(), col = Arm) +
    geom_segment(aes(xend = 50, yend = USUBJID |> fct_rev()), linewidth = 1, alpha = 0.5) +
    geom_point(aes(shape = BV), size = 2) +
    # geom_text(aes(label = note), size = 2) +
    scale_color_manual("", values = get_arm_colors()) +
    scale_shape_manual(values = c(16, 1)) +
    scale_y_discrete(
      "",
      breaks = pid_breaks$USUBJID,
      labels = pid_breaks$pid_label
    ) +
    xlab("Percentile rank of BC\nto own pre-MTZ microbiota") +
    theme(
      axis.text.y = element_text(size = axis_text_y_size)
    ) +
    
    plot_layout(ncol = 2, widths = c(2, 1))
  
}

show_examples <- function(res){
  example_pids <- 
    res$BC_all_others |> 
    filter(pid_label != "") |>
    arrange(pid_label) |> 
    mutate(USUBJID = USUBJID |> fct_drop() |> fct_inorder()) |> 
    pull(USUBJID) |> 
    unique()
  
  ASV_p <- 
    get_assay_long_format(
      mae = res$mae, assayname = "ASV_16S_filtered_p",
      feature_name = "ASV", values_name = "prop"
    ) |> 
    filter(
      USUBJID %in% example_pids,
      AVISITN %in% c(0, res$target_visit),
      prop > 0
    ) |> 
    select(USUBJID, AVISITN, ASV, prop) |> 
    mutate(
      USUBJID = USUBJID |> factor(levels = example_pids)
    )
  
  ASV_p_agg <- 
    ASV_p |> 
    group_by(ASV) |>
    mutate(max_prop = max(prop)) |> 
    ungroup() |> 
    mutate(
      ASV_label = 
        case_when(
          max_prop < 0.1 ~ "Other ASVs",
          TRUE ~ ASV
        )
    ) |> 
    group_by(USUBJID, AVISITN, ASV_label) |>
    summarize(prop = sum(prop), .groups = "drop")
  
  ASV_p_agg <- 
    ASV_p_agg |> 
    left_join(
      res$BC_all_others |> 
        select(USUBJID, pid_label) |> 
        distinct(),
      by = join_by(USUBJID)
    ) |> 
    left_join(
      res$df |> 
        select(USUBJID, BC_pre_MTZ_target, percentile_rank) |> 
        distinct(),
      by = join_by(USUBJID)
    ) |> 
    mutate(
      BC_text = str_c("BC: ", BC_pre_MTZ_target |> round(2)),
      PR_text = str_c("Perc. rank: ", percentile_rank |> round(0), "%")
    )
  
  
  ASV_p_agg |> 
    ggplot() +
    aes(x = AVISITN |> get_visit_labels(), y = prop, fill = ASV_label) +
    facet_grid(. ~ pid_label + BC_text + PR_text) +
    geom_col(col = "white") +
    scale_fill_manual(
      "ASV", 
      values = get_ASV_colors(ASV_p_agg$ASV_label |> unique() |> sort()),
      guide = guide_legend(ncol = 2, direction = "vertical")
    ) +
    xlab("") +
    scale_y_continuous(
      "Relative abundance",
      labels = scales::percent_format()
    ) +
    theme(
      legend.key.height = unit(0.3, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.margin = margin(t = -20, unit = "pt"),
      strip.text.x = element_text(margin = margin(t = 1, b = 1, unit = "pt")),
    )
}

BC_and_PR_by_history <- function(df, target_visit){
  
  c(
    var_by_history(df = df, var = "BC_pre_MTZ_target", target_visit = target_visit),
    var_by_history(df = df, var = "percentile_rank", target_visit = target_visit)
    ) |> 
    wrap_plots(nrow = 2, ncol = 4)
}


var_by_history <- function(df, var, target_visit){
  map(
    .x = c("prop_Lacto_post_MTZ", "delta_prop_Lacto", "prop_Lacto_max", "BC_pre_post"), # , "largest_BC_by_w8" "BC_pre_post_abs"
    var_by_history_var,
    df = df, var = var, target_visit = target_visit
  )
}

var_by_history_var <- function(df, var, target_visit, history_var){
  if (!var %in% colnames(df)) {
    stop(str_c("Variable ", var, " not found in the data frame."))
  }
  if (!history_var %in% colnames(df)) {
    stop(str_c("Variable ", history_var, " not found in the data frame."))
  }
  
  df$var <- df[[var]]
  
  if (var == "BC_pre_MTZ_target") {
    label <- 
      str_c(
        "Bray-Curtis dissimilarity\nbetween ASV rel. ab. at the\npre-MTZ and the ",
        get_visit_labels(target_visit) |> str_to_lower()," visit"
      )
  } else if (var == "percentile_rank") {
    label <- 
      str_c(
        "Percentile rank\nof BC dissimilarity to\nown pre-MTZ microbiota"
      )
    df$var <- df[[var]]/100
  } else{
    label <- var
  }
  
  df$history_var <- df[[history_var]]
  
  if (history_var == "prop_Lacto_post_MTZ") {
    xlabel <- "post-MTZ Lacto. rel. ab."
  } else if (history_var == "delta_prop_Lacto") {
    xlabel <- "diff. Lacto. rel. ab.\n(post-MTZ - pre-MTZ)"
  } else if (history_var == "prop_Lacto_max") {
    xlabel <- str_c("max Lacto. rel. ab. between\npost-MTZ (incl.) and ",target_visit |> get_visit_labels() |> str_to_lower(), "(excl.) visit")
  } else if (history_var == "largest_BC_by_w8") {
    xlabel <- "Largest BC between pre-MTZ and\nall subsequent visit until week 8 (incl.)"
  } else if (history_var == "BC_pre_post_abs") {
    xlabel <- "BC dissimilarity between\npre-MTZ and post-MTZ visit\n(absolute abundances)"
  } else if (history_var == "BC_pre_post") {
    xlabel <- "BC dissimilarity between\npre-MTZ and post-MTZ visit"
  } else {
    xlabel <- history_var
  }
  
  df |> 
    ggplot() +
    aes(x = history_var, y = var) +
    geom_point() +
    geom_smooth(method = "glm", formula = 'y ~ x', method.args = list(family = "quasibinomial")) +
    ylab(label) +
    xlab(xlabel) 
  
}


make_figures_recurrent_BV_composition <- function(res){
  
  
  BC_distributions <- make_distribution_figures(df = res$df, var = "BC_pre_MTZ_target", target_visit = res$target_visit)
  PR_distributions <- make_distribution_figures(df = res$df, var = "percentile_rank", target_visit = res$target_visit)
  
  BC_and_PR <- BC_and_PR_each_participant(BC_all_others = res$BC_all_others, df = res$df, target_visit = res$target_visit)
  
  examples <- show_examples(res = res)
  
  BC_and_PR_by_history <- BC_and_PR_by_history(df = res$df, target_visit = res$target_visit)
  
  MST <-  
    plot_MST(distances = res$BC_distances, sample_data = res$ASV_V0Vt) +
    ggtitle("MST based on Bray-Curtis distance")
  
  main_figure <- 
    free(BC_and_PR[[1]]) + free(BC_and_PR[[2]] + guides(shape = "none")) +
    free(examples) + 
    BC_distributions$g_by_arm_and_BV +
    PR_distributions$g_by_arm_and_BV +
    plot_layout(
      heights = c(2, 1),
      widths = c(3, 2, 2, 2),
      design = 
        "
        ABCC
        ABDE
      "
    ) +
    plot_annotation(tag_levels = "A")
  
  list(
    BC_distributions = BC_distributions, 
    PR_distributions = PR_distributions,
    BC_and_PR = BC_and_PR,
    examples = examples,
    BC_and_PR_by_history = BC_and_PR_by_history,
    MST = MST,
    main_figure = main_figure
  )  
}


make_summary_stat_table <- function(res){
  
  tmp <- 
    res$df |> 
    arrange(percentile_rank) |> 
    filter(!is.na(percentile_rank)) |> 
    mutate(
      cat = 
        case_when(
          percentile_rank == min(percentile_rank, na.rm = TRUE) ~ "Most similar to self (min perc. rank)",
          percentile_rank < 50 ~ "More similar to self than to others (> min. but < 50th perc. rank)",
          percentile_rank >= 50 ~ "More similar to others than to self (>= 50th perc. rank)",
        ) |> fct_inorder()
    ) 
  
  bind_rows(
    tmp |> dplyr::count(cat) |> mutate(Arm = "All"),
    tmp |> dplyr::count(Arm, cat)
  ) |> 
    group_by(Arm) |> 
    mutate(
      N = sum(n),
      perc = n / N * 100
    ) |> 
    ungroup() |> 
    rowwise() |>
    mutate(
      CI = 
        str_c(
          100 * prop.test(n, N)$conf.int[1] |> round(2), "% - ",
          100 * prop.test(n, N)$conf.int[2] |> round(2), "%"
        ),
      value = 
        str_c(
          n, " (", perc |> round(), "%) [", CI, "]"
        )
    ) |> 
    select(Arm, cat, value) |>
    pivot_wider(names_from = Arm, values_from = value) |>
    gt::gt(caption = "Number and percentage of participant by similarity category.") 
}


make_summary_stat_table_v2 <- function(res){
  
  tmp <- 
    res$df |> 
    arrange(percentile_rank) |> 
    filter(!is.na(percentile_rank)) |> 
    mutate(
      cat = 
        case_when(
          # percentile_rank == min(percentile_rank, na.rm = TRUE) ~ "Most similar to self (min perc. rank)",
          percentile_rank < 50 ~ "More similar to self than to others (< 50th perc. rank)",
          percentile_rank >= 50 ~ "More similar to others than to self (>= 50th perc. rank)",
        ) |> fct_inorder()
    ) 
  
  bind_rows(
    tmp |> dplyr::count(cat) |> mutate(Arm = "All"),
    tmp |> dplyr::count(Arm, cat)
  ) |> 
    group_by(Arm) |> 
    mutate(
      N = sum(n),
      perc = n / N * 100
    ) |> 
    ungroup() |> 
    rowwise() |>
    mutate(
      CI = 
        str_c(
          100 * prop.test(n, N)$conf.int[1] |> round(2), "% - ",
          100 * prop.test(n, N)$conf.int[2] |> round(2), "%"
        ),
      value = 
        str_c(
          n, " (", perc |> round(), "%) [", CI, "]"
        )
    ) |> 
    select(Arm, cat, value) |>
    pivot_wider(names_from = Arm, values_from = value) |>
    gt::gt(caption = "Number and percentage of participant by similarity category.") 
}



