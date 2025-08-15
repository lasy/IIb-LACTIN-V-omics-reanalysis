get_topic_colors <- function(topic_names){

  possible_colors <-
    c("steelblue","green3", "chartreuse3", "seagreen3","mediumseagreen", "forestgreen", "springgreen3", "springgreen4", "seagreen",
                 "violet","deeppink","orange", "darkorange",
                 "indianred1", "indianred3",
                 "red","red3", "turquoise3",
                 "cadetblue2", "lightblue1", "lightblue", "lightskyblue")

  # topic_colors <-
  #   bind_rows(
  #     tibble(topic = "I", color = "dodgerblue3"),
  #     tibble(topic = "II", color = "navy"),
  #     tibble(topic = "III", color = "seagreen3"),
  #     tibble(topic = "IV", color = "red"),
  #     tibble(topic = "IV-A", color = "deeppink"),
  #     tibble(topic = "IV-B", color = "orange"),
  #     tibble(topic = "IV-O.a", color = "red1"),
  #     tibble(topic = "IV-O.b", color = "red3"),
  #     tibble(topic = "V", color = "turquoise3"),
  #     tibble(topic = "VI", color = "lightblue1")
  #   )
  # 
  # topic_colors <-
  #   bind_rows(
  #     tibble(topic = "I", color = "orange1"),
  #     tibble(topic = "II", color = "deepskyblue"), # "#4FC3F7" # very similar
  #     tibble(topic = "III", color = "forestgreen"),
  #     tibble(topic = "IV", color = "dodgerblue2"),
  #     tibble(topic = "IV-A", color = "#CD93D8"), # plum would work too // burlywood2
  #     tibble(topic = "IV-B", color = "dodgerblue3"),
  #     tibble(topic = "IV-O.a", color = "slateblue3"), # #FDD835
  #     tibble(topic = "IV-O.b", color = "violetred4" ),
  #     tibble(topic = "V", color = "tomato"),
  #     tibble(topic = "VI", color = "cadetblue2")
  #   )
  
  topic_colors <-
    bind_rows(
      tibble(topic = c("I", "L. crispatus"), color = "orange"),
      tibble(topic = c("I.b"), color = "goldenrod1"),
      tibble(topic = c("I.a"), color = "darkorange1"),
      tibble(topic = c("I.c"), color = "tan2"), #  "darkgoldenrod3", "tan2", "goldenrod3"
      tibble(topic = c("II", "L. gasseri"), color = "gold"), 
      tibble(topic = c("III", "L. iners"),  color = "seagreen3"),   # forestgreen
      tibble(topic = c("III-B"), color = "seagreen4"), 
      tibble(topic = c("IV", "Gardnerella (g)", "G. s./l. topic"), color = "dodgerblue2"),
      tibble(topic = c("IV-A", "Ca. L. v. (BVAB1) topic"), color = "lightskyblue"), #  #CD93D8  # turquoise3
      tibble(topic = "Prevotella (g)", color = "plum2"), #  #CD93D8  # turquoise3
      tibble(topic = c("IV-B", "IV-B.a"), color = "dodgerblue3"), # dodgerblue2 # dodgerblue3
      tibble(topic = "IV-B.b", color = "dodgerblue4"),
      tibble(topic = c("IV-O.a", "P. bivia topic"), color = "slateblue4"),
      tibble(topic = "Fannyhessea (g)", color = "slateblue4"),
      tibble(topic = c("IV-O.b", "P. amnii topic"), color = "plum"),
      tibble(topic = "Megasphera (g)", color = "yellow2"),
      tibble(topic = c('Ca. Lachnocurva vag. ("BVAB1")'), color = "lightskyblue"),
      tibble(topic = c('other non-Lacto. A'), color = "deeppink2"),
      tibble(topic = c('other non-Lacto. B'), color = "pink"),
      tibble(topic = c("V", "L. jensenii"), color = "tomato"),
      tibble(topic = c("VI", "Lactobacillus (other)", "Other L."), color = "tomato4")
    )
  

 
  topic_colors$color[match(topic_names, topic_colors$topic)]

}
