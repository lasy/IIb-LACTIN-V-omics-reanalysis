
get_mtb_coefs_table <- function(res, boot){
  
  coefs <- get_mtb_coef(res = res, boot = boot)
  
  sign_coefs <- 
    coefs |> 
    mutate(signif = (sign(lo) == sign(up))) |> 
    filter(signif)
  
  sign_coefs <- 
    sign_coefs |> 
    select(Yvar, block, variable, value, lo, up) |>
    group_by(Yvar) |> 
    arrange(Yvar, block, value) |>
    dplyr::rename(Block = block, Variable = variable, Estimate = value, `95% CI lower` = lo, `95% CI upper` = up, Endpoint = Yvar)
  
  sign_coefs |> gt(row_group_as_column = TRUE) |> fmt_number(decimals = 2)
  
}
