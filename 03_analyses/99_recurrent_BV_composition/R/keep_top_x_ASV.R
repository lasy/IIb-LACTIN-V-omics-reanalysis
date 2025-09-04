
keep_top_x_ASV <- function(data_list, x){
  new_data_list <- data_list
  
  j <- order(colSums(data_list$B), decreasing = TRUE)[1:x]
  
  new_data_list$J <- x %>% as.integer()
  if ("s" %in% names(new_data_list)) new_data_list$s <- data_list$s[j]
  new_data_list$A <- data_list$A[,j]
  new_data_list$B <- data_list$B[,j]
  new_data_list$N_A <- new_data_list$A %>% rowSums() %>% as.integer()
  new_data_list
}

keep_top_x_samples <- function(data_list, i){
  new_data_list <- data_list
  
  new_data_list$N <- length(i)
  
  new_data_list$A <- data_list$A[i,]
  new_data_list$B <- data_list$B[i,]
  new_data_list$N_A <- new_data_list$A %>% rowSums() %>% as.integer()
  new_data_list
}
