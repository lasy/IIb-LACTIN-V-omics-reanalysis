
get_sex_summary_AD <- function(){

  ADSX <- readxl::read_xlsx(stringr::str_c(ADaM_dir, "ADSX.xlsx"), guess_max = 10000)

  sex_summary <-
    ADSX %>%
    filter(PARCAT1 == "Follow-up Sexual History") %>%
    filter(str_detect(PARAM, "sex") | str_detect(PARAM,"partner")) |> 
    select(USUBJID, AVISITN, PARAM, AVALC)

  sex_summary
}
