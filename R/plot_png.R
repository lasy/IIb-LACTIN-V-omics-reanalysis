plot_png <- function(path, padding = 0){
  image <- magick::image_read(path)
  image_ratio <- magick::image_info(image)$height / magick::image_info(image)$width 
  
  ggplot() + 
    coord_fixed(ratio = image_ratio) +
    ggpubr::background_image(image) + 
    # theme_minimal() +
    theme(
      panel.border = element_blank(),
      plot.margin = ggplot2::margin(t = padding, l = padding, r = 0, b = padding, unit = "pt")
      )
  
}

