
get_block_colors <- function(block_names){
  
  possible_colors <- 
    c(
      "indianred1", "coral", "gold1", "olivedrab2", "seagreen3", "darkseagreen3",
                  "cadetblue2", "skyblue2", "lightslateblue", "deeppink",
                  "seagreen3", "dodgerblue3", "mediumpurple", "orchid3", 
                  "aquamarine2", "green4"
    )
  

  
  block_colors <-
    bind_rows(
      tibble(block = "Arm", color = "orange"),
      tibble(block = "Demographics", color = "orchid3"),
      tibble(block = "Birth control", color = "mediumpurple"),
      
      tibble(block = "Adherence", color = "darkslategray3"),
      tibble(block = "Antibiotic use", color = "deepskyblue3"),
      tibble(block = "Perturbations", color = "skyblue2"),
      tibble(block = "Sexual behavior", color = "dodgerblue3"),
      tibble(block = "Last event", color = "dodgerblue4"), 
      
      tibble(block = "Vag. environment", color = "darkslategray"),
      tibble(block = "Vag. environment post-MTZ", color = "darkslategray"),
      tibble(block = "Microbiota (16S topics)", color = "seagreen3"),
      tibble(block = "Microbiota (16S topics) pre-MTZ", color = "seagreen3"),
      tibble(block = "Microbiota (16S topics) post-MTZ", color = "seagreen3"),
      tibble(block = "Microbiota (16S topics) week 12", color = "seagreen3"),
      tibble(block = "Microbiota (16S topics) next visit", color = "seagreen3"),
      tibble(block = "Cytokines", color = "seagreen4"),
      tibble(block = "Cytokines pre-MTZ", color = "seagreen4"),
      tibble(block = "Cytokines post-MTZ", color = "seagreen4"),
      
      tibble(block = "BV", col = "red2")
      
    )
  
  block_colors$color[match(block_names, block_colors$block)]
  
}
