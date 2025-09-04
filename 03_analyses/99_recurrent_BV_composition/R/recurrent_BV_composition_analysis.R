
recurrent_BV_composition_analysis <- function(mae, target_visit){
  target_visit <- match.arg(
    target_visit |> as.character(), 
    choices = target_visits |> as.character(), 
    several.ok = FALSE
  ) |> as.numeric()
  
  ######
  # Inclusion criteria
  # - has data pre-MTZ and at the target visit
  # - has less than 50% Lactobacillus
  df <- 
    MultiAssayExperiment::colData(mae) |> 
    as_tibble() |> 
    # inclusion criteria 1
    filter(
      has_V0,
      AVISITN == target_visit
    ) |> 
    select(USUBJID, BV) |> 
    distinct() |> 
    # retrieve the Lactobacillus proportions at the target visit and define "mBV" (molecular BV)
    full_join(
      get_assay_wide_format(mae, "prop_Lacto") |> 
        filter(AVISITN == target_visit) |> 
        mutate(prop_Lacto_target = assay$prop_Lacto) |> 
        select(USUBJID, prop_Lacto_target) |> 
        mutate(mBV = prop_Lacto_target < 0.5),
      by = join_by(USUBJID)
    ) |>
    # retrieve the arm
    left_join(
      MultiAssayExperiment::colData(mae) |> 
        as_tibble() |> select(USUBJID, ARM) |> distinct() |> dplyr::rename(Arm = ARM),
      by = join_by(USUBJID)
    ) |> 
    select(USUBJID, Arm, everything())
  
 # inclusion criteria 2
  df <- df |> filter((BV == "Yes") | mBV)
  outcome_table <- 
    df |> dplyr::count(BV, mBV) |> 
    mutate(
      BV = BV |> str_replace_na("Unknown"),
      mBV = ifelse(mBV, "< 50% Lactobacillus", "≥ 50% Lactobacillus")
    ) |> 
    pivot_wider(names_from = mBV, values_from = n, values_fill = 0) |> 
    gt::gt(
      caption = "Number of participants included in the analysis by BV diagnosis and Lactobacillus dominance at the week 24 visit."
    )
  
  #######
  # Compute distances to the pre-MTZ microbiota composition
  
  ASV_prop <-  
    get_assay_wide_format(mae, "ASV_16S_filtered_p") |> 
    filter(USUBJID %in% df$USUBJID, AVISITN %in% c(0, target_visit)) |> 
    select(USUBJID, AVISITN, assay)
  
  ASV_target <- 
    ASV_prop |> 
    filter(AVISITN == target_visit) |> 
    mutate(ASV_p_target = assay |> as.matrix() |> set_rownames(USUBJID)) |> 
    select(USUBJID, ASV_p_target)
  
  ASV_pre_MTZ <- 
    ASV_prop |> 
    filter(AVISITN == 0) |> 
    mutate(ASV_p_pre_MTZ = assay |> as.matrix() |> set_rownames(USUBJID)) |> 
    select(USUBJID, ASV_p_pre_MTZ)
  
  df <- 
    df |> 
    select(-any_of(c("ASV_p_target", "ASV_p_pre_MTZ"))) |>
    left_join(ASV_target, by = "USUBJID") |>
    left_join(ASV_pre_MTZ, by = "USUBJID") 
  
  rm(ASV_prop, ASV_target, ASV_pre_MTZ)
  
  #####
  # Compute the Bray-Curtis distance between the pre-MTZ and target visit microbiota
  
  df <- 
    df |> 
    mutate(
      BC_pre_MTZ_target = 1 - 2 * rowSums(pmin(df$ASV_p_target, df$ASV_p_pre_MTZ, na.rm = TRUE)) / rowSums(df$ASV_p_target + df$ASV_p_pre_MTZ)
    )
  
  ########
  # Compute the BC distance between the target visit and the pre-MTZ microbiota of all other participants
  BC_all_others <- 
    ValenciaR::BC(
      df$ASV_p_target, 
      df$ASV_p_pre_MTZ |> as.data.frame() |> drop_na()
    ) |> 
    as.data.frame() |> 
    rownames_to_column("USUBJID") |>
    pivot_longer(
      cols = -USUBJID, 
      names_to = "pre_MTZ_USUBJID", 
      values_to = "BC"
    ) |> 
    mutate(
      same_USUBJID = USUBJID == pre_MTZ_USUBJID,
      same_USUBJID_label = ifelse(same_USUBJID, "Same participant", "Other participants") |> fct_infreq() |> fct_rev()
    ) |> 
    arrange(!same_USUBJID, BC) |> 
    mutate(USUBJID = USUBJID |> fct_inorder(), order_BC = USUBJID |> as.integer()) |> 
    group_by(USUBJID) |> 
    arrange(USUBJID, BC) |> 
    mutate(
      has_own_BC = any(same_USUBJID),
      BC_mean = mean(BC, na.rm = TRUE), # [!same_USUBJID]
      BC_sd = sd(BC, na.rm = TRUE), # [!same_USUBJID]
      z_score = (BC - BC_mean)/BC_sd,
      percentile_rank = ecdf(BC)(BC) * 100
    ) |> 
    ungroup() 
  
  ######
  # Add the percentile rank to the main dataframe
  df <- 
    df |> 
    select(-any_of("percentile_rank")) |> 
    left_join(
      BC_all_others |> 
        filter(same_USUBJID) |> 
        select(USUBJID, percentile_rank),
      by = join_by(USUBJID)
    )
  
  ########
  # Identifying interesting examples
  BC_all_others <- 
    BC_all_others |> 
    mutate(
      is_smallest_distance = 
        same_USUBJID & (BC == min(BC[same_USUBJID], na.rm = TRUE)),
      is_smallest_percentile_rank = 
        same_USUBJID & (percentile_rank == min(percentile_rank[same_USUBJID], na.rm = TRUE)),
      is_largest_distance_within_smallest_percentile_rank = 
        is_smallest_percentile_rank & (BC == max(BC[is_smallest_percentile_rank], na.rm = TRUE)),
      combo = 100*(1-BC) + percentile_rank,
      is_smallest_combo = same_USUBJID & (combo == min(combo[same_USUBJID], na.rm = TRUE)), 
      is_largest_distance = same_USUBJID & (BC == max(BC[same_USUBJID], na.rm = TRUE)),
      is_largest_percentile_rank = same_USUBJID & (percentile_rank == max(percentile_rank[same_USUBJID], na.rm = TRUE))
    ) |> 
    group_by(USUBJID) |>
    mutate(
      note = 
        case_when(
          any(is_smallest_distance) ~ "smallest distance",
          any(is_largest_distance_within_smallest_percentile_rank) ~ "smallest percentile rank with largest BC",
          any(is_smallest_combo) ~ "smallest combo",
          any(is_largest_distance) ~ "largest distance",
          any(is_largest_percentile_rank) ~ "largest percentile rank",
          TRUE ~ ""
        )
    ) |> 
    ungroup() |> 
    arrange(order_BC) |> 
    mutate(
      note = note |> factor() |> fct_inorder() |> fct_relevel("", after = Inf),
      pid_label = LETTERS[note |> as.integer()] |> str_replace(LETTERS[nlevels(note)], "")
    )
  
  ##########
  # Add potential explanatory variables for recurring to the same composition
  # - the post-MTZ microbiota proportion of Lactobacillus and 
  # - delta proportion of Lactobacillus compared to pre-MTZ
  # - the max proportion of Lactobacillus between the post-MTZ (incl.) and the target visit (excl.)
  # - the largest distance between pre-MTZ and all other visits preceding and excluding week 12
  # - the BC dissimilarity between the pre-MTZ and post-MTZ microbiota considering relative abundances scaled by total bacterial load
  
  df <- 
    df |> 
    select(-any_of(c("prop_Lacto_pre_MTZ", "prop_Lacto_post_MTZ", "delta_prop_Lacto"))) |>
    left_join(
      get_assay_wide_format(mae, "prop_Lacto") |> 
        filter(AVISITN == 0) |> 
        mutate(prop_Lacto_pre_MTZ = assay$prop_Lacto) |> 
        select(USUBJID, prop_Lacto_pre_MTZ),
      by = join_by(USUBJID)
    ) |> 
    left_join(
      get_assay_wide_format(mae, "prop_Lacto") |> 
        filter(AVISITN == 1) |> 
        mutate(prop_Lacto_post_MTZ = assay$prop_Lacto) |> 
        select(USUBJID, prop_Lacto_post_MTZ),
      by = join_by(USUBJID)
    ) |> 
    mutate(
      delta_prop_Lacto = prop_Lacto_post_MTZ - prop_Lacto_pre_MTZ
    )
  
  # Add the max proportion of Lactobacillus between the post-MTZ (incl.) and the target visit (excl.)
  
  df <- 
    df |> 
    select(-any_of(c("prop_Lacto_max"))) |>
    left_join(
      get_assay_wide_format(mae, "prop_Lacto") |> 
        filter(AVISITN != 0, AVISITN < target_visit) |> 
        mutate(prop_Lacto = assay$prop_Lacto) |> 
        select(USUBJID, AVISITN, prop_Lacto) |> 
        arrange(USUBJID, AVISITN) |> 
        group_by(USUBJID) |> 
        summarize(prop_Lacto_max = max(prop_Lacto, na.rm = TRUE), .groups = "drop"),
      by = join_by(USUBJID)
    ) 
  
  # Add the largest distance between pre-MTZ and all other visits preceding and excluding week 12
  df <- 
    df |> 
    select(-any_of(c("largest_BC_by_w8"))) |>
    left_join(
      df |> select(USUBJID, ASV_p_pre_MTZ) |> 
        left_join(
          get_assay_wide_format(mae, "ASV_16S_filtered_p") |> 
            filter(AVISITN != 0, AVISITN < 4) |> 
            mutate(ASV_p_other = assay[, colnames(df$ASV_p_pre_MTZ)] |> as.matrix()) |> 
            select(USUBJID, AVISITN, ASV_p_other) |> 
            arrange(USUBJID, AVISITN),
          by = join_by(USUBJID)
        ) |> 
        mutate(
          BC = 1 - 2 * rowSums(pmin(ASV_p_pre_MTZ, ASV_p_other, na.rm = TRUE)) / rowSums(ASV_p_pre_MTZ + ASV_p_other)
        ) |>  
        group_by(USUBJID) |> 
        summarize(
          largest_BC_by_w8 = ifelse(any(!is.na(BC)), max(BC, na.rm = TRUE), NA), 
          .groups = "drop"
          ),
      by = join_by(USUBJID)
    ) 
  
  # the BC dissimilarity between the pre-MTZ and post-MTZ microbiota considering relative abundances scaled by total bacterial load
  df <- 
    df |> 
    select(-any_of(c("BC_pre_post","BC_pre_post_abs"))) |>
    left_join(
      df |> 
        select(USUBJID, ASV_p_pre_MTZ) |> 
        left_join(
          get_assay_wide_format(mae, "ASV_16S_filtered_p") |> 
            filter(AVISITN == 1) |> 
            mutate(ASV_p_post_MTZ = assay[, colnames(df$ASV_p_pre_MTZ)] |> as.matrix()) |> 
            select(USUBJID, AVISITN, ASV_p_post_MTZ) |> 
            arrange(USUBJID, AVISITN),
          by = join_by(USUBJID)
        ) |>
        left_join(
          mae@colData |> 
            as_tibble() |> 
            select(USUBJID, AVISITN, LOAD) |> 
            filter(AVISITN == 1) |> 
            distinct() |> 
            mutate(
              post_MTZ_load = LOAD |> replace_na(median(LOAD, na.rm = TRUE)),
              pre_MTZ_load = mae$LOAD[mae$AVISITN != 1] |> median(na.rm = TRUE)
              ) |> 
            select(USUBJID, post_MTZ_load, pre_MTZ_load),
          by = join_by(USUBJID)
        ) |> 
        mutate(
          ASV_pre_MTZ_abs = ASV_p_pre_MTZ * pre_MTZ_load,
          ASV_post_MTZ_abs = ASV_p_post_MTZ * post_MTZ_load
        ) |>
        mutate(
          BC_pre_post_abs = 1 - 2 * rowSums(pmin(ASV_pre_MTZ_abs, ASV_pre_MTZ_abs, na.rm = TRUE)) / rowSums(ASV_pre_MTZ_abs + ASV_pre_MTZ_abs),
          BC_pre_post = 1 - 2 * rowSums(pmin(ASV_p_pre_MTZ, ASV_p_post_MTZ, na.rm = TRUE)) / rowSums(ASV_p_pre_MTZ + ASV_p_post_MTZ)
        ) |> 
        select(USUBJID, BC_pre_post_abs, BC_pre_post),
      by = join_by(USUBJID)
    )
  
  ########
  # Computing the minimum spanning tree (MST) and associated permutation test
    
  ASV_V0Vt <- 
    get_assay_wide_format(mae, "ASV_16S_filtered") |> 
    filter(
      USUBJID %in% df$USUBJID,
      AVISITN %in% c(0, target_visit)
      ) |> 
    mutate(
      ID = paste(USUBJID, AVISITN, sep = "_"),
    )
  
  BC_distances <- 
    vegan::vegdist(
      ASV_V0Vt$assay %>% as.matrix() %>% set_rownames(ASV_V0Vt$ID), 
      method = "bray"
    )
  
  
  ps_ASV_V0Vt <- 
    phyloseq::phyloseq(
      otu_table(ASV_V0Vt$assay, taxa_are_rows = FALSE),
      sample_data(ASV_V0Vt %>% select(-assay))
    )
  
  gt <- 
    graph_perm_test(
      ps_ASV_V0Vt, sampletype = "USUBJID",
      distance = "(A+B-2*J)/(A+B)",
      type = "mst",  nperm = 1000
    )

  
  list(mae = mae, target_visit = target_visit, df = df, outcome_table = outcome_table, BC_all_others = BC_all_others, ASV_V0Vt = ASV_V0Vt, BC_distances = BC_distances, gt = gt)
}
