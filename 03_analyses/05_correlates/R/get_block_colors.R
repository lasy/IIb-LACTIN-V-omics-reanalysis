
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
      tibble(block = c("Demographics", "Demogr."), color = "mediumpurple"),
      
      
      tibble(block = "Vag. env. pre-MTZ", color = "deeppink4"),
      tibble(block = "Microbiota pre-MTZ", color = "deeppink1"), 
      tibble(block = "Cytokines (r) pre-MTZ", color = "deeppink3"), 
      
      tibble(block = "Coloniz. cat. prev. v.", color = "darkslategray"),
      tibble(block = "Vag. env. prev. v.", color = "aquamarine4"),
      tibble(block = "Microbiota (r) prev. v.", color = "aquamarine2"),
      tibble(block = "Cytokines (r) prev. v.", color = "aquamarine3"), 
    

      tibble(block = "Birth control", color = "steelblue4"),
      tibble(block = "Adherence", color = "deepskyblue3"),
      tibble(block = "Antibiotics", color = "cadetblue2"),
      tibble(block = "Perturbations", color = "skyblue2"), 
      tibble(block = "Sexual behavior", color = "dodgerblue3")
    )
   
  block_colors$color[match(block_names, block_colors$block)]
  
}