
plot_participant_data <- 
  function(ID, mae, events, visits, selected_events, selected_cytokines) {
    
  
    # data
    p_events <- 
      events %>% 
      filter(
        USUBJID == ID,
        CATEGORY %in% selected_events | VARIABLE %in% selected_events
      )
    
    p_visits <- 
      visits %>% filter(USUBJID == ID) %>% 
      mutate(VISIT_CAT = ifelse(round(AVISITN) %in% 5:6, "phone","in-person"))
    
    # cat("here 1\n")
    
    cytokines <- 
      get_assay_long_format(
        mae[, which(mae$USUBJID == ID),], "cytokine_transformed", 
        add_colData = TRUE,
        feature_name = "cytokine", values_name = "transformed_concentration"
      ) %>% 
      filter(cytokine %in% selected_cytokines)
    
    # cat("here 2\n")
    
    
    topics <- 
      get_assay_long_format(
        mae[, which(mae$USUBJID == ID),], "c_topics_16S_8", 
        add_colData = TRUE,
        feature_name = "topic", values_name = "prop"
      )
    topic_names <- this_participant_topics$topic %>% levels()
    
    x_limits <- range(p_events$DAY/7) + c(-1,1)
    x_breaks <- seq(-4,24, by = 4)
    x_minor_breaks <- x_limits[1]:x_limits[2]
    
    clin <- 
      colData(mae) %>% as.data.frame() %>% filter(USUBJID == ID) %>% 
      select(ARM, RACEGR2) %>% distinct()
  
    title <- str_c(ID, " (",clin$RACEGR2 ," | ", clin$ARM," arm)")
    
    # plots 
    
    # cat("here 3\n")
    
    
    g_events <- 
      plot_participant_events(
        p_events = p_events, p_visits = p_visits, title = FALSE, x_scale = FALSE
      ) +
      scale_x_continuous(
        "", breaks = x_breaks, minor_breaks = x_minor_breaks, limits = x_limits
      )
    
    
    
    # cat("here 4\n")
    
    
    load_limits <- mae$LOAD[mae$LOAD > 0] %>% log10() %>% range(., na.rm = TRUE) %>% multiply_by(c(0.99, 1.01))
    
    g_topics <- 
      ggplot(topics, 
             aes(x = DAY/7, y = prop, fill = topic)) +
      geom_vline(data = p_visits, aes(xintercept = DAY/7, linetype = VISIT_CAT), col = "slategray2") +
      scale_linetype("Visit type") +
      geom_bar(stat = "identity", aes(alpha = log10(LOAD))) +
      scale_alpha(
        "log10(bacterial load)", range = c(0.2, 1), 
        limits = load_limits , breaks = c(ceiling(load_limits[1]), floor(load_limits[2]))
      ) +
     #  geom_point(aes(y = 1.1, size = log10(LOAD)), col = "black") +
      scale_fill_manual("Topic",
        breaks = topic_names,
        values = get_topic_colors(topic_names),
        labels = get_topic_labels(topic_names)
      )  +
      facet_grid("topic\nproportions" ~ .) +
      theme(strip.text.y = element_text(color = "black", angle = 0, hjust = 0))  +
      ylab("") +
      guides(fill = guide_legend(ncol = 2)) +
      scale_x_continuous(
        "", breaks = x_breaks, minor_breaks = x_minor_breaks, limits = x_limits
      ) 
    
    # cat("here 5\n")
    
    g_cytok <-
      ggplot(cytokines, 
             aes(x = DAY/7, y = transformed_concentration, col = cytokine )) +
      geom_vline(data = p_visits, aes(xintercept = DAY/7, linetype = VISIT_CAT), col = "slategray2") +
      scale_linetype("Visit type") +
      geom_line(alpha = 0.5) +
      geom_point() +
      ylab("") +
      facet_grid("transformed\ncytokine\nconcentrations" ~ .) +
      theme(strip.text.y = element_text(color = "black", angle = 0, hjust = 0)) +
      scale_x_continuous(
        "weeks since study start", 
        breaks = x_breaks, minor_breaks = x_minor_breaks, limits = x_limits
      ) +
      scale_color_manual(
        "Cytokine", breaks = selected_cytokines, 
        values = get_cytokine_colors(selected_cytokines)
      )
    
    g_events + theme(legend.position = "right") + ggtitle(title) +
      g_topics +
      g_cytok + 
      plot_layout(nrow = 3, guides = "collect", heights = c(1.2, 1, 1))
    
  }