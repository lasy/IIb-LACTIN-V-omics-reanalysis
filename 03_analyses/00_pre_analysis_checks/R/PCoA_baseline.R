

#' Performs a PCoA and PERMANOVA test on baseline 16S composition data
#'
#' @param mae a `MultiAssayExperiment` object 
#' @param assay_name a `character` giving the name of the assay to use
#' @param visit a `numeric` giving the visit number we're interested in
#' @param included_participants a `character` vector of participant IDs to include in the analysis
#'
#' @return a `patchwork` series of 3 ggplot2 objects
#' @export
#'
#' @import patchwork
PCoA_baseline <- function(mae, assay_name, visit, included_participants, distance = "bray") {
  
  distance_name <- ifelse(distance == "bray", "Bray-Curtis", "XXX SPECIFY DISTANCE XXXX")
  if (assay_name == "tax_16S_p") {
    assay_name_print <- "relative abundances aggregated by taxonomic assignment"
  } else if (assay_name == "ASV_16S_filtered") {
    assay_name_print <- "ASV relative abundances"
  } else {
    assay_name_print <- assay_name
  }
  
  
  composition_data <- MultiAssayExperiment::assay(mae, assay_name) %>% t()
  
  j <- 
    which(
      (mae$AVISITN == visit) & 
        (mae$USUBJID %in% included_participants) & 
        (mae$Barcode %in% rownames(composition_data))
      )
  composition_data <- composition_data[mae$Barcode[j],]
  coldata <- tibble(Barcode = mae$Barcode[j], ARM = mae$ARM[j])
  
  BC_dist <- vegan::vegdist(composition_data, method = distance)
  mds <- cmdscale(BC_dist, k = 4, eig = TRUE)
  
  g_eig <-
    ggplot(tibble(k = 1:20, eig = mds$eig[1:20]), aes(x = k, y = eig)) +
    geom_bar(stat = "identity") +
    labs(title = "Top eigenvalues")
  
  mds_df <-
    tibble(
      PCo1 = mds$points[, 1],
      PCo2 = mds$points[, 2],
      PCo3 = mds$points[, 3],
      PCo4 = mds$points[, 4],
      Barcode = rownames(mds$points)
    ) |> 
    left_join(coldata, by = "Barcode")
  
  fit <- vegan::adonis2(composition_data ~ ARM, data = coldata, permutations = 999, method = distance)
  
  g_arms <-
    ggplot(mds_df, aes(x = PCo1, y = PCo2, col = ARM)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom") +
    annotate(
      "label", x = Inf, y = Inf, hjust = 1, vjust = 1,
      label = str_c("p: ", (fit$`Pr(>F)`[1]) %>% round(2))
    ) +
    scale_color_manual("",values = get_arm_colors()) +
    ggtitle("Scores by study arm")
  
  g_arms_34 <-
    ggplot(mds_df, aes(x = PCo3, y = PCo4, col = ARM)) +
    geom_point() +
    coord_fixed() +
    scale_color_manual("",values = get_arm_colors())  +
    guides(col = "none")
  
  mds_topics <- 
    mds_df |> 
    left_join(
      get_assay_long_format(
        mae, "c_topics_16S_8", add_colData = FALSE, add_rowData = TRUE,
        feature_name = "topic", values_name = "proportion"
        ), 
      by = "Barcode") 
  
  g_topics <-
    ggplot(mds_topics, aes(x = PCo1, y = PCo2, col = topic_label, alpha = proportion)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom") +
    scale_color_manual(
      "", values = get_topic_colors(mds_topics$topic_label |> levels()),
      guide = guide_legend(nrow = 2)
      ) +
    scale_alpha(range = c(0, 1)) +
    guides(alpha = "none", col = "none") +
    ggtitle("Scores by topic")

  g_topics_34 <-
    ggplot(mds_topics, aes(x = PCo3, y = PCo4, col = topic_label, alpha = proportion)) +
    geom_point() +
    coord_fixed() +
    scale_color_manual(
      "", values = get_topic_colors(mds_topics$topic_label |> levels()),
      guide = guide_legend(nrow = 2)
    ) +
    scale_alpha(range = c(0, 1)) +
    guides(alpha = "none")
  
  
  g_eig + 
    (g_arms + g_arms_34 + plot_layout(nrow = 1, guides = "collect")) + 
    (g_topics + g_topics_34 + plot_layout(nrow = 1, guides = "collect")) +
    plot_layout(nrow = 1, width = c(0.7, 2, 2)) +
    plot_annotation(
    title = str_c("Microbiota composition at the ", visit |> get_visit_labels(), " visit"),
    subtitle = str_c('PCoA using ',distance_name,' distances computed on ', assay_name_print),
    caption = "p-value obtained from PERMANOVA as implemented in (`vegan::adonis2`)"
  )
  
  
  
}



#' Performs a PCoA and PERMANOVA test on baseline 16S composition data
#'
#' @param mae a `MultiAssayExperiment` object 
#' @param assay_name a `character` giving the name of the assay to use
#' @param visit a `numeric` giving the visit number we're interested in
#' @param included_participants a `character` vector of participant IDs to include in the analysis
#'
#' @return a `patchwork` series of 3 ggplot2 objects
#' @export
#'
#' @import patchwork
PCoA_baseline_deprecated_1 <- function(mae, assay_name, visit, included_participants, distance = "bray") {
  
  composition_data <- MultiAssayExperiment::assay(mae, assay_name) %>% t()
  
  j <- 
    which(
      (mae$AVISITN == visit) & 
        (mae$USUBJID %in% included_participants) & 
        (mae$Barcode %in% rownames(composition_data))
    )
  composition_data <- composition_data[mae$Barcode[j],]
  coldata <- tibble(Barcode = mae$Barcode[j], ARM = mae$ARM[j])
  
  BC_dist <- vegan::vegdist(composition_data, method = distance)
  mds <- cmdscale(BC_dist, eig = TRUE)
  
  g_eig <-
    ggplot(tibble(k = 1:20, eig = mds$eig[1:20]), aes(x = k, y = eig)) +
    geom_bar(stat = "identity") +
    labs(title = "Top eigenvalues", subtitle = str_c('MDS using "',distance,'"'))
  
  mds_df <-
    tibble(
      PCo1 = mds$points[, 1],
      PCo2 = mds$points[, 2],
      Barcode = rownames(mds$points)
    ) |> 
    left_join(coldata, by = "Barcode")
  
  fit <- vegan::adonis2(composition_data ~ ARM, data = coldata, permutations=999, method=distance)
  
  g_arms <-
    ggplot(mds_df, aes(x = PCo1, y = PCo2, col = ARM)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom") +
    annotate(
      "label", x = Inf, y = Inf, hjust = 1, vjust = 1,
      label = str_c("p: ", (fit$`Pr(>F)`[1]) %>% round(2))
    ) +
    scale_color_manual("",values = get_arm_colors()) +
    ggtitle("PCoA by study arm")
  
  mds_df <- 
    mds_df |> 
    left_join(get_assay_wide_format(mae, "CST", add_colData = FALSE), by = "Barcode") |> 
    mutate(CST = assay$CST)
  g_cst <-
    ggplot(mds_df, aes(x = PCo1, y = PCo2, col = CST)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = get_topic_colors(mds_df$CST |> unique() |> sort()))
  
  
  g_eig + g_arms + g_cst + plot_annotation(
    title = str_c("Microbiota composition at visit ", visit),
    caption = "p-value obtained from PERMANOVA as implemented in (`vegan::adonis2`)"
  )
  
  
  
}



#' Performs a PCoA and PERMANOVA test on baseline 16S composition data
#'
#' @param species a `matrix` with the counts for each sample (rows) and each species (columns)
#' @param CSTs a `data.frame` giving the CST membership and other sample info such as `ARM`, `AVISITN`, and `EOSSTT`
#' @param visit a `numeric` giving the visit number we're interested in
#'
#' @return a `patchwork` series of 3 ggplot2 objects
#' @export
#'
#' @import patchwork
PCoA_baseline_deprecated_2 <- function(species, CSTs, visit) {
  
  
  j <- which((CSTs$AVISITN == visit) & (CSTs$EOSSTT == "COMPLETED") & !is.na(CSTs$ARM))
  species_data <- species[CSTs$Barcode[j],]
  
  BC_dist <- vegan::vegdist(species_data, method = "bray")
  mds <- cmdscale(BC_dist, eig = TRUE)
  
  g_eig <-
    ggplot(tibble(k = 1:20, eig = mds$eig[1:20]), aes(x = k, y = eig)) +
    geom_bar(stat = "identity") +
    ggtitle(stringr::str_c("Visit: ", visit))
  
  
  mds_df <-
    tibble(
      PCo1 = mds$points[, 1],
      PCo2 = mds$points[, 2],
      Barcode = rownames(mds$points)
    ) %>%
    left_join(CSTs, by = "Barcode")
  
  
  fit <- vegan::adonis2(species_data ~ ARM, data = CSTs[j, ], permutations=999, method="bray")
  
  g_arms <-
    ggplot(mds_df, aes(x = PCo1, y = PCo2, col = ARM)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom") +
    annotate(
      "label", x = Inf, y = Inf, hjust = 1, vjust = 1,
      label = str_c("p: ", (fit$`Pr(>F)`[1]) %>% round(2))
    ) +
    scale_color_manual(values = c("cornflowerblue","darkorange"))
  
  g_cst <-
    ggplot(mds_df, aes(x = PCo1, y = PCo2, col = CST)) +
    geom_point() +
    coord_fixed() +
    theme(legend.position = "bottom")
  
  
  g_eig + g_arms + g_cst
}


