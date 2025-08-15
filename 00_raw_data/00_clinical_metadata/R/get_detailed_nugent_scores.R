
get_detailed_nugent_scores <- function() {


  RS <- readxl::read_xlsx(str_c(SDTM_dir,"RS.xlsx"), guess_max = 1000)

  RS <-
    RS %>%
    filter(RSCAT == "NUGENT01") %>%
    select(USUBJID, `Visit Number`, RSTEST, RSORRES) %>%
    mutate(
      AVISITN = `Visit Number` - 100,
      RSTEST = RSTEST %>% str_remove("01")
    ) %>%
    select(-`Visit Number`) %>%
    distinct() %>% # to remove duplicated rows of participant "STI.00386"
    mutate(
      test =
        case_when(
          str_detect(RSTEST, "Lactobacillus") ~ "NUGENT_L",
          str_detect(RSTEST, "Gardnerella") ~ "NUGENT_G",
          str_detect(RSTEST, "Mobiluncus") ~ "NUGENT_M"
        )
    ) %>%
    select(-RSTEST) %>%
    filter(!is.na(test)) %>%
    pivot_wider(
      id_cols = c(USUBJID, AVISITN),
      names_from = test,
      values_from = RSORRES
    ) %>%
    mutate(
      across(
        .cols = starts_with("NUGENT"),
        .fns = as.integer
      )
    )


  variable_info <-
    bind_rows(
      tibble(var = "NUGENT_L", label = "Lactobacillus spp. Nugent Score"),
      tibble(var = "NUGENT_G", label = "Gardnerella/Bacteroides Nugent Score"),
      tibble(var = "NUGENT_M", label = "Mobiluncus Nugent Score")
    ) %>%
    mutate(
      type = "integer",
      group = "Nugent"
    )

  list(
    detailed_nugent_scores = RS,
    variable_info = variable_info
  )

}
