
plot_amsel_heatmaps <-
  function(mae,  add_prop_Lacto = FALSE, 
           col_low = "dodgerblue", col_high = "indianred2", col_mid = "gray90"
          ){
    
    get_colors <- colorRampPalette(colors = c(col_low, col_mid, col_high))
    amsel_criteria <- c("AMSEL", "pH","CLUE_CELLS", "DISCH", "AMINE_WT")
    
    
    if (add_prop_Lacto) {
      df <- 
        get_assay_wide_format(mae, "prop_Lacto") %>% 
        mutate(`prop Lacto` = assay$prop_Lacto) %>% 
        filter(!is.na(AMSEL)) %>% 
        arrange(-`prop Lacto`) %>% 
        mutate(Barcode = Barcode %>% factor(., unique(Barcode))) %>% 
        select(Barcode, `prop Lacto`, all_of(amsel_criteria))
        
      g_prop_Lacto <- 
        ggplot(df, aes(x = Barcode, y = "prop Lacto", fill = `prop Lacto`)) +
        geom_tile() + 
        scale_fill_gradient2(
          "", low = col_high, high = col_low, mid = col_mid, midpoint = 0.5
        ) +
        ylab("") + xlab("") +
        scale_x_discrete(breaks = NULL)
      
    } else {
      df <- 
        MultiAssayExperiment::colData(mae) %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        filter(!is.na(AMSEL)) %>% 
        arrange(AMSEL, pH, CLUE_CELLS) %>% 
        mutate(Barcode = Barcode %>% factor(., unique(Barcode))) %>% 
        select(Barcode, all_of(amsel_criteria))
    }
    
    
    
    g_AMSEL <- 
      ggplot(df, aes(x = Barcode, y = "Amsel score", fill = AMSEL)) +
      geom_tile() + 
      scale_fill_manual("", values = get_colors(nlevels(df$AMSEL))) +
      ylab("") + xlab("") +
      scale_x_discrete(breaks = NULL)
    
    g_pH <- 
      ggplot(df, aes(x = Barcode, y = "pH", fill = pH)) +
      geom_tile() + 
      scale_fill_gradient2("",
                           low = col_low, high = col_high, mid = col_mid, midpoint = 4.6
      ) +
      ylab("") + xlab("") +
      scale_x_discrete(breaks = NULL)
    
    g_clue_cells <- 
      ggplot(df, aes(x = Barcode, y = "Clue cells", fill = CLUE_CELLS)) +
      geom_tile() + 
      scale_fill_gradient2("",
                           low = col_low, high = col_high, mid = col_mid, midpoint = 20
      ) +
      ylab("") + xlab("") +
      scale_x_discrete(breaks = NULL)
    
    g_disch <- 
      ggplot(df, aes(x = Barcode, y = "Discharge", fill = DISCH)) +
      geom_tile() + 
      scale_fill_manual("", values = get_colors(nlevels(df$DISCH))) +
      ylab("") + xlab("") +
      scale_x_discrete(breaks = NULL)
    
    g_Amine <- 
      ggplot(df, aes(x = Barcode, y = "Amine W.T.", fill = AMINE_WT)) +
      geom_tile() + 
      scale_fill_manual("",values = get_colors(nlevels(df$AMINE_WT))) +
      ylab("") + xlab("samples") +
      scale_x_discrete(breaks = NULL)
    
    if (add_prop_Lacto) {
      p <- g_prop_Lacto + g_AMSEL + g_pH + g_clue_cells + g_disch + g_Amine
    } else {
      p <- g_AMSEL + g_pH + g_clue_cells + g_disch + g_Amine
    }
    
   p + plot_layout(ncol = 1) & theme(legend.direction = "horizontal")
    
    
  }
