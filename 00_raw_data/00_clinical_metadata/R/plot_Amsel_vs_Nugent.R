
plot_Amsel_vs_Nugent <- function(visits) {

  ggplot(
    visits %>%
      filter(
        BV %in% c("Yes","No"),
        !is.na(NUGENT),
        !is.na(AMSEL)
      ) %>%
      mutate(
        Diagnosis =
          ifelse(BV == "Yes", "BV","Not BV") %>%
          factor(., levels = c("Not BV","BV")),
        AMSEL = AMSEL %>% as.character() %>% as.integer(),
        NUGENT = NUGENT %>% as.character() %>% as.integer()
      ),
    aes(x = AMSEL,
        y = NUGENT,
        col = Diagnosis)) +
    annotate(
      "rect",
      xmin = 2.5, xmax = 4.5, ymin = 3.5, ymax = 10.5,
      fill = get_fct_colors("BV")[1], alpha = 0.1
      ) +
    geom_jitter(height = 0.1, width = 0.1, alpha = 0.5, size = 0.5) +
    scale_color_manual(values = get_fct_colors("BV")[2:1]) +
    scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
    scale_x_continuous(breaks = 0:4, minor_breaks = NULL)# +
    #facet_grid(. ~ Diagnosis) +
    # guides(col = "none")
}
