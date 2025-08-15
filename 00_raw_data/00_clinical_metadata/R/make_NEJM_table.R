


make_NEJM_table <- function(subjects, visits, target_visit) {
  
  BV_data <- 
    subjects |> 
    select(USUBJID, ARM) |> 
    expand_grid(AVISITN = c(4,7)) |> 
    full_join(
      visits |> select(USUBJID, AVISITN, PIPV, BV, BV_LOCF, BV_WC) |> 
        left_join(subjects |> select(USUBJID, ARM), by = join_by(USUBJID)), 
      by = join_by(ARM, USUBJID, AVISITN)) |> 
    arrange(ARM, USUBJID, AVISITN)
  
  BV_subjects <- 
    BV_data |> 
    filter((AVISITN > 1) & (AVISITN <= target_visit)) |> 
    group_by(ARM, USUBJID) |> 
    summarize(
      any_rBV = 
        case_when(
          any(BV == "Yes") ~ "Yes",
          !is.na(BV[AVISITN == target_visit]) & any(BV == "No") ~ "No",
          is.na(BV[AVISITN == target_visit]) ~ "Missing",
          TRUE ~ "PROBLEM"
        ) |> factor(levels = c("Yes", "No", "Missing", "PROBLEM")),
      .groups = "drop"
    ) |> 
    arrange(ARM, any_rBV)
  
  plot <- 
    BV_data |> 
    left_join(BV_subjects, by = join_by(USUBJID, ARM)) |> 
    filter(AVISITN <= target_visit) |>
    ggplot(aes(x = AVISITN |> factor(), y = USUBJID, fill = BV)) +
    geom_tile() +
    facet_grid(ARM + any_rBV ~ ., scales = "free", space = "free") +
    xlab("Visits")  +
    theme(
      axis.text.y = element_text(size = 2)
    ) 
  
  
  table <- 
    BV_subjects |> 
    count(ARM, any_rBV) |> 
    pivot_wider(names_from = ARM, values_from = n) |> 
    janitor::adorn_totals("row")
  
  
  list(plot = plot, table = table)
}