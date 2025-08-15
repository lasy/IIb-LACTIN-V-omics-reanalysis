
get_last_event_before_visit <- function(visits){
  summary <- 
    visits |> 
    select(USUBJID, AVISITN, starts_with("LAST")) |> 
    pivot_longer(
      cols = starts_with("LAST"), names_to = "event", names_prefix = "LAST_", 
      values_to = "DAYS"
    ) |>
    filter(!is.na(DAYS), event != "SEX") |>
    arrange(USUBJID, AVISITN, DAYS, event) |>
    group_by(USUBJID, AVISITN) |> 
    mutate(min_DAYS = min(DAYS, na.rm = TRUE)) |> 
    ungroup() |>
    filter(DAYS == min_DAYS) |> 
    select(USUBJID, AVISITN, event) |>
    mutate(value = TRUE) |> 
    pivot_wider(names_from = event, values_from = value, 
                names_glue = "{event}_IS_LAST", values_fill = FALSE) |>
    right_join(visits |> select(USUBJID, AVISITN), by = join_by(USUBJID, AVISITN)) |> 
    arrange(USUBJID, AVISITN) 
  
  var_info <- 
    tibble(var = colnames(summary)[-(1:2)]) |>
    mutate(
      var_label = case_when(
        str_detect(var, "DOSE") ~ "Taking a dose",
        str_detect(var, "DOUCH") ~ "Douching",
        str_detect(var, "M_IS_LAST") ~ "Menstrual bleeding",
        str_detect(var, "UNPROT") ~ "Condomless-sex",
        str_detect(var, "PROT") ~ "Sex with condom",
        str_detect(var, "STER") ~ "Taking a steroid drug",
        str_detect(var, "AFUN") ~ "Taking an antifungal drug",
        str_detect(var, "O_ABIO") ~ "Taking an oral antibiotic",
        str_detect(var, "V_ABIO") ~ "Taking a vaginal antibiotic"
      ),
      label = str_c(var_label, " is the last event before visit"),
      type = "logical", group = "Last event"
    ) |> 
    select(-var_label)
  
  list(summary = summary, variable_info = var_info)
}
