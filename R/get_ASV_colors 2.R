
get_ASV_colors <- function(ASV_label){
  tibble(ASV = ASV_label) |> 
    mutate(
      color = 
        case_when(
          str_detect(ASV, "iners") ~ get_topic_colors("III"),
          str_detect(ASV, "crispatus") ~ get_topic_colors("I"),
          str_detect(ASV, "jensenii") ~ get_topic_colors("V"),
          str_detect(ASV, "swidsinskii") ~ get_topic_colors("IV-B"),
          str_detect(ASV, "piotii") ~ "dodgerblue4",
          str_detect(ASV, "Gardnerella") ~ "blue1",
          str_detect(ASV, "amnii") ~ get_topic_colors("P. amnii topic"),
          str_detect(ASV, "bivia") ~ get_topic_colors("P. bivia topic"),
          str_detect(ASV, "BVAB1") ~ get_topic_colors("Ca. L. v. (BVAB1) topic"),
          str_detect(ASV, "Prevotella") ~ "slateblue2",
          str_detect(ASV, "Sneathia") ~ "plum3",
          str_detect(ASV, "Gemella") ~ "purple1",
          str_detect(ASV, "Fannyhessea") ~ "dodgerblue1",
          str_detect(ASV, "Megasphaera") ~ "steelblue1",
          str_detect(ASV, "Escherichia") ~ "red4",
          str_detect(ASV, "Other") ~ "gray",
          TRUE ~ "red2"
        )
    ) |> 
    pull(color)
}
