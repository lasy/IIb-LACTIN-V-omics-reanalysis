


get_days_since_last_dose <- function(visits, events){
  

  days_since_last_dose <- 
    visits |> 
    filter(AVISITN > 1) |> 
    select(USUBJID, AVISITN, DAY) |> 
    rename(visit_DAY = DAY) |> 
    full_join(
      events |> 
        filter(CATEGORY == "Doses") |> 
        select(USUBJID, DAY, VARIABLE),
      by = join_by(USUBJID),
      relationship = "many-to-many"
    ) |> 
    filter(DAY < visit_DAY) |> 
    mutate(days_since_dose = visit_DAY - DAY) |>
    group_by(USUBJID, AVISITN) |> 
    summarize(LAST_DOSE = min(days_since_dose), .groups = "drop") 
  
  var_info <-
    tibble(var = "LAST_DOSE", label = "Days since last dose") |> 
    mutate(
      type = "integer",
      group = "Doses"
    )
  
  list(
    days_since_last_dose = days_since_last_dose,
    variable_info = var_info
  )
}


get_days_since_last_dose_ADEFDY <- function() {
  
  ADEFDY <-
    readxl::read_xlsx(stringr::str_c(ADaM_dir, "ADEFDY.xlsx"), guess_max = 10000)
 
    ADEFDY |> 
    filter(PARAM == "Days Since Last Dose") |> 
    mutate(LAST_DOSE = AVAL |> as.integer()) |> 
    select(USUBJID, AVISITN, LAST_DOSE)
}
