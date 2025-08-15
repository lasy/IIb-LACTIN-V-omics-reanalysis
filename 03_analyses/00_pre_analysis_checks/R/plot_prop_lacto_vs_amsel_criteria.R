
plot_prop_lacto_vs_amsel_criteria <- function(mae, main_col = "coral1", limit_col = "steelblue1") {
  
  prop_Lacto <- 
    get_assay_wide_format(mae, "prop_Lacto") %>% 
    filter(!is.na(AMSEL)) %>% 
    mutate(`prop Lacto` = assay$prop_Lacto)
  
  g_AMSEL <- 
    ggplot(prop_Lacto, aes(x = AMSEL, y = `prop Lacto`)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = main_col) +
    xlab("Amsel score")
  
  g_pH <- 
    ggplot(prop_Lacto, aes(x = pH, y = `prop Lacto`)) +
    geom_vline(xintercept = 4.5, col = limit_col, linewidth = 2) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_smooth(fill = main_col, col = main_col)
  
  g_clue_cells <- 
    ggplot(prop_Lacto, aes(x = CLUE_CELLS, y = `prop Lacto`)) +
    geom_vline(xintercept = 20, col = limit_col, linewidth = 2) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_smooth(fill = main_col, col = main_col) +
    xlab("% Clue Cells")
  
  g_disch <- 
    ggplot(prop_Lacto, aes(x = DISCH, y = `prop Lacto`)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = main_col) +
    xlab("Discharge")
  
  g_amine <- 
    ggplot(prop_Lacto, aes(x = AMINE_WT, y = `prop Lacto`)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = main_col) +
    xlab("Amine Whiff test")
  
  g_AMSEL + plot_spacer() + g_pH + g_clue_cells + g_disch + g_amine +
    plot_layout(nrow = 3)
  
  
}