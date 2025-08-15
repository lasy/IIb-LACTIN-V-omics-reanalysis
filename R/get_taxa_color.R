


#' Get colors for taxa
#'
#' This function returns a color for each taxa in the input string vector.
#' Colors are based on the color of the topic to which they primarily belong.
#' 
#' @param taxa a character vector with the taxa name for which one wants colors
#'
#' @return a character vector of the same length than the input `taxa` with the colors for each input taxa


get_taxa_colors <- function(taxa){
  betas <- get_beta_long(mae, "c_topics_16S_8")
  main_topic <- 
    betas %>% 
    arrange(taxa, -prop) %>% 
    group_by(taxa) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>% 
    arrange(topic, -prop) %>% 
    mutate(topic_color = get_topic_colors(topic)) 
  
  main_topic <- 
    main_topic %>% 
    bind_cols(DescTools::ColToHsv(main_topic$topic_color) %>% t() %>% as_tibble()) %>% 
    group_by(topic) %>% 
    mutate(N = n(), n = row_number(), new_v = v - v*(n-1)/N) %>% 
    ungroup() %>% 
    mutate(color = hsv(h = h, s = s, v = new_v))
  
  tmp <- 
    tibble(taxa = taxa) %>% 
    left_join(main_topic, by = join_by(taxa)) 

  tmp$color
  
}


# From Joseph
# c(">50% L. crispatus" = "#E69F00",">50% other Lactobacillus"="#009E73", "<50% Lactobacillus"="#0072B2")

# From Seth

# subcst:
# bind_rows(
#   tibble(subCST = "I-A", color = "orange2"),
#   tibble(subCST = "I-B", color = "orange4"),
#   tibble(subCST = "II", color = "plum1"),
#   tibble(subCST = "III-A", color = "forestgreen"),
#   tibble(subCST = "III-B", color = "darkgreen"),
#   tibble(subCST = "IV-A", color = "dodgerblue3"),
#   tibble(subCST = "IV-B", color = "violetred4"),
#   tibble(subCST = "IV-C0", color = "#4FC3F7"),
#   tibble(subCST = "IV-C1", color = "burlywood2"),
#   tibble(subCST = "IV-C2", color = "darkolivegreen3"),
#   tibble(subCST = "IV-C3", color = "lightskyblue3"),
#   tibble(subCST = "IV-C4", color = "black"), # ?
#   tibble(subCST = "V", color = "tomato")
# )

# 
# taxa:
# 
# bind_rows(
#   tibble(taxa = "Lactobacillus crispatus", color = "orange2"),
#   tibble(taxa = "Lactobacillus iners", color = "forestgreen"),
#   tibble(taxa = "Lactobacillus jensenii", color = "tomato"),
#   tibble(taxa = "Lactobacillus gasseri", color = "#4FC3F7"),
#   tibble(taxa = "Gardnerella", color = "dodgerblue3"),
#   tibble(taxa = "Prevotella", color = "violetred4"),
#   tibble(taxa = "BVAB1", color = "burlywood2"),
#   tibble(taxa = "", color = "#9FA8DA"),
#   tibble(taxa = "Lactobacillus crispatus", color = "lightskyblue3"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   tibble(taxa = "Lactobacillus crispatus", color = "burlywood2"),
#   
# )
# c("orange2",
#   "forestgreen",
#   "tomato",
#   "#4FC3F7",
#   "dodgerblue3",
#   "violetred4",
#   "burlywood2",
#   "#9FA8DA",
#   "lightskyblue3",
#   "#FDD835",
#   "maroon3",
#   "plum1",
#   "darkolivegreen3",
#   "#004D40",
#   "#CD93D8",
#   "slateblue3",
#   "snow3",
#   "grey48")



