get_cytokine_colors <- function(cytokine_names){
  cytokine_colors <- 
    bind_rows(
      tibble(cytokine = "ITAC", color = "gray20"),
      tibble(cytokine = "IFNg", color = "gray20"),
      tibble(cytokine = "IL-10", color = "gray20"),
      tibble(cytokine = "MIP-3a", color = "gray20"),
      tibble(cytokine = "IL-13", color = "gray20"),
      tibble(cytokine = "IL-17", color = "gray20"),
      tibble(cytokine = "IL-1a", color = "coral"),
      tibble(cytokine = "IL-1b", color = "red2"),
      tibble(cytokine = "MIG", color = "gray20"),
      tibble(cytokine = "IL-21", color = "gray20"),
      tibble(cytokine = "IL-4", color = "gray20"),
      tibble(cytokine = "IL-23", color = "gray20"),
      tibble(cytokine = "IL-6", color = "gray20"),
      tibble(cytokine = "IL-8", color = "gray20"),
      tibble(cytokine = "IP-10", color = "mediumaquamarine"),
      tibble(cytokine = "MIP-1a", color = "steelblue2"),
      tibble(cytokine = "MIP-1b", color = "gray20"),
      tibble(cytokine = "TNFa", color = "gray20")
    )
  
  cytokine_colors$color[match(cytokine_names, cytokine_colors$cytokine)]
  
}
