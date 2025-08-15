
plot_slda_gamma <- function(slda_mod, clin) {
   
  gamma <- get_slda_gamma(slda_mod, clin)
  ggplot(gamma, aes(x = prop, fill = coef)) +
    geom_histogram(binwidth = 0.025) +
    facet_grid(. ~ k) +
    scale_fill_gradient2()
  
}

get_slda_gamma <- function(slda_mod, clin) {
  
  slda_mod$document_sums %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(clin$Barcode) %>% 
    mutate(k = row_number(), coef = slda_mod$coefs) %>% 
    pivot_longer(-c(k,coef), names_to = "Barcode", values_to = "counts") %>% 
    group_by(Barcode) %>% 
    mutate(tot_counts = sum(counts), prop = counts / tot_counts) %>% 
    ungroup() %>% arrange(Barcode) %>% 
    left_join(clin, by = join_by(Barcode))
  
}

plot_slda_gamma_association <- function(slda_mod, clin, var = "BV"){
  gamma <- get_slda_gamma(slda_mod, clin)
  gamma$x <- gamma[,var] %>% unlist()
  
  g <-  
    ggplot(gamma, aes(x = x, y = prop, col = coef)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_gradient2() +
    facet_grid(. ~ k) +
    xlab(var)
  
  if (is.factor(gamma$x)) {
    g <-  g + geom_boxplot(outlier.shape = NA, alpha = 0.5)
  } else {
    g <- g + geom_smooth(method = "lm", formula = "y ~ x")
  }
  
  g
 
}