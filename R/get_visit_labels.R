get_visit_labels <- function(visit_numbers, long = FALSE){
  visits <- c("Pre-MTZ", "Post-MTZ", str_c("Week ", seq(4, 24, by = 4))) 
  label_levels <- str_c(rep(visits, each = 2), rep(c(""," (suppl. visit)"), length(visits)))
  tmp <- 
    tibble(AVISITN = visit_numbers %>% as.character() %>% as.numeric()) %>% 
    mutate(
      visit_number = floor(AVISITN),
      week = visits[visit_number + 1],
      visit_label =
        case_when(
          visit_number == AVISITN ~ week,
          TRUE ~ str_c(week, " (suppl. visit)")
        )
    )
  if (long) 
    res <- tmp$visit_label %>% factor(., levels = label_levels)
  else 
    res <- tmp$week %>% factor(., levels = visits)
  res
}



get_visit_colors <- function(visits){
  if (is.numeric(visits)) {
    visit_labels <- get_visit_labels(visits)
  } else if (is.factor(visits) | is.character(visits)) {
    if (any(str_detect(visits %>% as.character(), "[a-z]"))) {
      possible_visit_labels <- 
        get_visit_labels(seq(0,7, by = 0.5), long = TRUE) %>% 
        levels()
      if (!all(visits %in% possible_visit_labels)) 
        stop(
          str_c("visits must be one of ", 
                str_c(possible_visit_labels, collapse = ", "))
          )
      visit_labels <- visits
    } else {
      visit_labels <- get_visit_labels(visits)
    }
  } 
  
  tmp <- 
    tibble(
    visit_label = visit_labels %>% as.character()
  ) %>% 
    mutate(
      color = 
        case_when(
          str_detect(visit_labels, "Pre-MTZ") ~ "red4",
          str_detect(visit_labels, "Post-MTZ") ~ "orangered",
          str_detect(visit_labels, " 4") ~ "steelblue1",
          str_detect(visit_labels, " 8") ~ "steelblue2",
          str_detect(visit_labels, " 12") ~ "steelblue3",
          str_detect(visit_labels, " 16") ~ "gray80",
          str_detect(visit_labels, " 20") ~ "gray60",
          str_detect(visit_labels, " 24") ~ "steelblue4",
        )
    )
  tmp$color
}