
plot_visit_status <- function(visits){
  
  ggplot(visits, 
         aes(x = AVISITN |> factor(), y = USUBJID, fill = Status)) +
    geom_tile() +
    facet_grid(str_wrap(SITENAME, 15) + ARM ~ ., scales = "free", space = "free") +
    xlab("Visits") +
    theme(
      axis.text.y = element_text(size = 3),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
}
