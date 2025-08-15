

get_n_samples_statistics <- function(mae, assay_list, barcodes = NULL){
  
  if (is.null(barcodes)) barcodes <- mae$Barcode
  
  mae_selected_assays_and_barcodes <- 
    mae[,barcodes, assay_list] |> 
    MultiAssayExperiment::intersectColumns()
  
  colData <- 
    MultiAssayExperiment::colData(mae_selected_assays_and_barcodes) |> 
    as_tibble() |> 
    select(USUBJID, ARM, AVISITN) 
  
  
  participants_meeting_criteria <- 
    colData |>
    group_by(USUBJID, ARM) |>
    summarize(
      `has data at week 12` = any(AVISITN == 4),
      `has data at week 24` = any(AVISITN == 7),
      `has data pre-MTZ and at week 12` = sum(AVISITN %in% c(0, 4)) == 2,
      `has data pre-MTZ and at week 24` = sum(AVISITN %in% c(0, 7)) == 2,
      `has data pre-MTZ and post-MTZ` = sum(AVISITN %in% c(0, 1)) == 2,
      `has data pre-MTZ, post-MTZ, and at week 12` = sum(AVISITN %in% c(0, 1, 4)) == 3,
      `has data pre-MTZ, post-MTZ, and at week 24` = sum(AVISITN %in% c(0, 1, 7)) == 3,
      `has data pre-MTZ, post-MTZ, and at weeks 4, 8, 12` = sum(AVISITN %in% c(0:4)) == 5,
      `has data at all planned in-person visits` = sum(AVISITN %in% c(0:4,7)) == 6,
      .groups = "drop"
    )
  
  
  n_meeting_criteria <- 
    participants_meeting_criteria |> 
    pivot_longer(-c(USUBJID, ARM), names_to = "Criteria", values_to = "meets_criteria") |> 
    filter(meets_criteria) |> 
    count(Criteria, ARM) |> 
    pivot_wider(names_from = ARM, values_from = n) |> 
    arrange(-Placebo)
  
  
  n_meeting_criteria |> 
    mutate(
      `pre-MTZ` = ifelse(str_detect(Criteria, "pre-MTZ") | str_detect(Criteria, "all"), "X", ""),
      `post-MTZ` = ifelse(str_detect(Criteria, "post-MTZ") | str_detect(Criteria, "all"), "X", ""),
      `Week 4` = ifelse(str_detect(Criteria, " 4") | str_detect(Criteria, "all"), "X", ""),
      `Week 8` = ifelse(str_detect(Criteria, " 8") | str_detect(Criteria, "all"), "X", ""), 
      `Week 12` = ifelse(str_detect(Criteria, " 12") | str_detect(Criteria, "all"), "X", ""),
      `Week 24` = ifelse(str_detect(Criteria, " 24") | str_detect(Criteria, "all"), "X", "")
    ) |> 
    select(-Criteria) |>
    select(`pre-MTZ`, `post-MTZ`, `Week 4`, `Week 8`, `Week 12`, `Week 24`, everything())
  
}
