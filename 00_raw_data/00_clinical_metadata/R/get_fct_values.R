
get_fct_values <- function(x, fct_dict = factor_values){
  fct_dict$values[fct_dict$var == x]
}

get_fct_colors <- function(x, fct_dict = factor_values){
  fct_dict$colors[fct_dict$var == x]
}
