
plot_CC_cor <- function(cancor_object){
  tibble(
    `canonical variable` = seq_along(cancor_object$cor) %>% factor(),
    cor = cancor_object$cor
  ) %>% 
    ggplot(., aes(x = `canonical variable`, y = cor)) +
    geom_bar(stat = "identity", fill = "gray") +
    geom_text(aes(label = cor %>% round(., 2)), vjust = 0, nudge_y = 0.01)
}
