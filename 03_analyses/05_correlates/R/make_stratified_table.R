

make_stratified_table <- function(table_data, target_week, outcome_of_interest, var_desc) {
  
  table_title <- 
    str_c(
      "Demographics and behavior by ", 
      ifelse(outcome_of_interest == "rBV", "rBV oucome by","*L. crispatus* colonization at"),
      " week ", target_week," in both arms."
    )
  
  x <- 
    map(
    .x = var_desc$block |> unique(),
    .f = function(.x){
      table_data |> 
        filter(outcome_var == outcome_of_interest, week == target_week, !is.na(outcome)) |> 
        select(ARM, outcome, all_of(var_desc$var[var_desc$block == .x])) |>
        gtsummary::tbl_strata(
          strata = ARM,
          .tbl_fun = 
            ~ gtsummary::tbl_summary(
              data = .x,
              by = outcome,
              label = 1:nrow(var_desc) |> map(.f = ~ var_desc$desc[.x]) |> set_names(var_desc$var),
              type = list("total_new_partners" = "continuous", "n_douching" = "continuous", "pH" = "continuous"),
              statistic = list(
                all_continuous() ~ "{mean} ({min}-{max})",
                all_categorical() ~ "{n} ({p}%)"
              )
            ), # |> add_p(),
          .header = "**{strata}**, N = {n}"
        ) # |> bold_labels()
    }
  ) |> 
    tbl_stack(group_header = str_c("**", var_desc$block |> unique(), "**")) |> 
    as_gt() |> 
    gt::tab_header(title = md(table_title)) 
  
  x
}
