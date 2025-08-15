
plot_taxa_composition <- 
  function(mae, assayname, visit_nb, arm, n_species = 15, facet_by_endpoint = TRUE, add_BV = TRUE){
    
    microbiota <- 
      get_assay_long_format(
        mae, assayname = assayname, 
        values_name = "prop", feature_name = "Taxon"
      ) 
    
    top_taxa <- 
      microbiota |> 
      group_by(Taxon) |> 
      summarize(mean_prop = mean(prop)) |> 
      ungroup() |> 
      arrange(desc(mean_prop)) |> 
      slice_head(n = n_species)
    
    microbiota <- 
      microbiota |> 
      mutate(
        taxon_group = 
          ifelse(Taxon %in% top_taxa$Taxon, Taxon |> as.character(), "Other") |> 
          factor(levels = c(top_taxa$Taxon |> as.character(), "Other")) |> 
          fct_relevel("Lactobacillus crispatus")
      ) |> 
      group_by(USUBJID, ARM, AVISITN, BV, taxon_group) |> 
      summarize(prop = sum(prop), .groups = "drop") |> 
      ungroup()
    
    microbiota <- 
      microbiota |> 
      filter(AVISITN == visit_nb, ARM == arm) 
    
    microbiota <- 
      microbiota |> 
      group_by(USUBJID) |> 
      mutate(
        L_crisp_prop = prop[taxon_group == "Lactobacillus crispatus"],
        tot_Lacto_prop = sum(prop[str_detect(taxon_group, "Lactobacillus")]),
        endpoint = 
          case_when(
            L_crisp_prop >= 0.5 ~ "≥ 50% L. crisp.",
            tot_Lacto_prop >= 0.5 ~ "≥ 50% Lacto.",
            TRUE ~ "< 50% Lacto."
          ) |> factor(levels = c("≥ 50% L. crisp.", "≥ 50% Lacto.",  "< 50% Lacto."))
      ) |> 
      ungroup() |> 
      arrange(-L_crisp_prop, -tot_Lacto_prop) |> 
      mutate(USUBJID = USUBJID |> fct_inorder())
    
    colors <- 
      bind_rows(
        tibble(pattern = "crispatus", color = get_topic_colors("I")),
        tibble(pattern = "iners", color = get_topic_colors("III")),
        tibble(pattern = "jensenii", color = get_topic_colors("V")),
        tibble(pattern = "Lactobacillus", color = get_topic_colors("VI")),
        tibble(pattern = "Gardnerella", color = get_topic_colors("IV")), 
        tibble(pattern = "BVAB1", color = get_topic_colors("IV-A")),
        tibble(pattern = "bivia", color = get_topic_colors("IV-O.a")),
        tibble(pattern = "Sneathia amnii", color = get_topic_colors("IV-O.b")),
        tibble(pattern = "Megasphaera", color = "lightskyblue3"),
        tibble(pattern = "Fannyhessea vaginae", color = "dodgerblue4"),
        tibble(pattern = "Prevotella amnii", color = "slateblue2"),
        tibble(pattern = "Prevotella timonensis", color = "yellow"),
        tibble(pattern = "Prevotella sp_1", color = "purple"),
        tibble(pattern = "Sneathia sanguinegens", color = "plum4"),
        tibble(pattern = "Other", color = "gray")
      )
    
    actual_colors <- 
      tibble(taxon = levels(microbiota$taxon_group)) |> 
      mutate(
        color = 
          taxon |> 
          map_chr(~ colors$color[str_detect(.x, colors$pattern)] |> extract(1))
      )
    
    microbiota <- 
      microbiota |> 
      arrange(taxon_group, ARM) |> 
      mutate(
        taxon_group = 
          taxon_group |> 
          str_replace("Candidatus", "Ca.") |>
          str_replace("vaginae", "v.") |>
          str_replace("sp_", "sp. ") |>
          str_replace("swidsinskii_leopoldii","swid./leop.") |> 
          fct_inorder(),
        Arm = ARM |> str_replace("ACTIN","actin") |> fct_inorder()
        )
    
    y_lims <- c(0, 1 + 0.08 * add_BV) + c(-1, 1) * 0.001

    g <-
      microbiota |> 
      ggplot(aes(x = USUBJID, y = prop, fill = taxon_group)) +
      geom_col() +
      scale_fill_manual(
        "Taxon", 
        values = actual_colors$color,
        guide = guide_legend(direction = "vertical", ncol = 1)
      ) +
      labs(x = "Participant", y = "Relative abundance") +
      scale_y_continuous(
        labels = scales::percent, breaks = seq(0, 1, len = 5), limits = y_lims,
        expand = c(0, 0)
        ) +
      theme(
        axis.text.x = element_blank(),
        legend.position = "right"
      )
    
    if (facet_by_endpoint)
      g <- g + facet_grid(Arm ~ endpoint, scales = "free_x", space = "free")
    else
      g <- g + facet_grid(Arm ~ ., scales = "free")
    
    if (add_BV)
      g <- 
      g + 
      ggnewscale::new_scale_fill() +
      geom_tile(aes(y = 1.03, fill = BV), height = 0.04, col = "white") +
      scale_fill_manual(
        "BV diagnosis", values = c("Yes" = "red", "No" = "steelblue1"),
        labels = c("Positive", "Negative"),
        guide = guide_legend(direction = "vertical", ncol = 1, order = 1)
      ) 
    
    g
  }