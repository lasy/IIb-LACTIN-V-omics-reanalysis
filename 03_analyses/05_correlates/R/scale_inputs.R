
scale_inputs <- function(inputs, scaling = c("variable information", "uniform variables", "uniform blocks", "block rank",  "none"), verbose = TRUE){
  if (verbose) cat("Scaling inputs...")
  scaling <- match.arg(scaling)
  if (verbose) cat("(method:", scaling, ")\n")
  
  if (scaling == "variable information"){
    X <- inputs |> bind_cols() |> ade4::scalewt()
    X <- t(t(X) * var_weight(X))
    new_inputs <- back_to_blocks(X, inputs)
  } else if (scaling == "uniform variables") {
    X <- inputs |> bind_cols() |> ade4::scalewt()
    new_inputs <- back_to_blocks(X, inputs)
  } else if (scaling == "uniform blocks") {
    new_inputs <- map(inputs, ~.x |> ade4::scalewt() |> multiply_by(sqrt(1/ncol(.x))))
  } else if (scaling == "block rank") {
    new_inputs <- map(inputs, ~.x |> ade4::scalewt() |> multiply_by(block_rank(.x)))
  } else if (scaling == "none") {
    new_inputs <- inputs
  }
  new_inputs
}

back_to_blocks <- function(X, inputs){
  tmp <- tibble(
    variable = colnames(X),
    block = rep(names(inputs), map(inputs, ncol))
  )
  blo <- names(inputs)
  new_inputs <- 
    map(blo, ~tmp |> filter(block == .x) |> pull(variable)) |> 
    map(~X[, .] |> as.data.frame()) |> 
    set_names(blo)
  new_inputs
}

var_weight <- function(X){
  corX <- cor(X)
  diag(corX) <- NA
  n_cor <- 1 + colSums(corX^2, na.rm = TRUE)
  sqrt(1/n_cor)
}


block_rank <- function(X){
  P <- ncol(X)
  pca <- prcomp(X)
  eig <- pca$sdev^2/sum(pca$sdev^2)
  rel_eig <- eig/sum(eig)
  1:P |> weighted.mean(rel_eig) |> divide_by(mean(1:P)) |> sqrt()  
}

add_tiny_noise <- function(inputs){
  purrr::map(
    .x = inputs,
    .f = function(X){
      sds <- apply(X, 2, sd) + 1/1000
      tiny_noise <- sapply(seq_along(sds), function(i) rnorm(nrow(X), mean = 0, sd = sds[i] / 1000)) 
      X + tiny_noise
    }
  )
}
