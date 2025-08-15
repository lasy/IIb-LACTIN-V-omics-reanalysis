get_topic_labels <- function(topic_names){
    
 topic_labels <- tibble(
    topic = c("I","II","III","IV-A","IV-B","IV-O.a","IV-O.b","V","VI"),
    label = 
    c("I (L. crisp)","II (L. gasseri)","III (L. iners)", 
      "IV-A (non-Lacto)", "IV-B (non-Lacto)",
      "IV-O.a (non-Lacto)", "IV-O.b (non-Lacto)", 
      "V (L. jensenii)", "VI (other Lacto)"
    )
  )
 
 topic_labels$label[match(topic_names, topic_labels$topic)]
 
}

# get_topic_labels <- function(mae, topic_assay_name){
#   rowData(mae[[topic_assay_name]])$topic_label
# }