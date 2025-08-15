
get_BV_diagnosis <- function() {

  ADEF <- readxl::read_xlsx(stringr::str_c(ADaM_dir, "ADEF.xlsx"), guess_max = 10000)

  variable_info <-
    bind_rows(
      tibble(var = "BV", label = "BV diagnosis", type = "factor", group = "BV"),
      tibble(var = "BV_LOCF", label = "BV diagnosis [LOCF]", type = "factor", group = "BV"),
      tibble(var = "BV_WC", label = "BV diagnosis [WC]", type = "factor", group = "BV"),
      tibble(var = "SUCCESS_COL", label = "Successful colonization of L. crispatus", type = "logical"),
      tibble(var = "SUCCESS_COL_E", label = "Successful colonization of L. crispatus (endpoint)", type = "logical"),
      tibble(var = "LCRISP", label = "L. crispatus concentration", type = "double", group = "qPCR"),
      tibble(var = "CTV05", label = "CTV-05 (L. crispatus) concentration", type = "double", group = "qPCR"),
      tibble(var = "LACTO", label = "Lactobacillus concentration", type = "double", group = "qPCR"),
      tibble(var = "LOAD", label = "Total Bacteria", type = "double", group = "qPCR"),
      tibble(var = "DISCH", label = "Homogenous, thin, greayish-white discharge", type = "factor", group = "Amsel"),
      tibble(var = "pH", label = "pH", type = "double", group = "Amsel"),
      tibble(var = "AMINE_WT", label = "Amine Whiff Test", type = "factor", group = "Amsel"),
      tibble(var = "CLUE_CELLS", label = "Clue Cells (%)", type = "integer", group = "Amsel"),
      tibble(var = "AMSEL", label = "Amsel Criteria Score", type = "integer", group = "Amsel"),
      tibble(var = "NUGENT", label = "Nugent Score", type = "integer", group = "Nugent")
    )

  BV_values <- c("Yes", "No","Missing")
  BV_colors <- c("red","steelblue","gray70")

  factor_values <-
    bind_rows(
      tibble(var = "BV", values = BV_values, colors = BV_colors),
      tibble(var = "BV_LOCF", values = BV_values, colors = BV_colors),
      tibble(var = "BV_WC", values = BV_values, colors = BV_colors),
      tibble(var = "DISCH", values = c("Absent", "Present"), colors = BV_colors[1:2]),
      tibble(var = "AMINE_WT", values = c("Negative", "Positive"), colors = BV_colors[1:2]),
      tibble(var = "NUGENT", values =  (0:10) %>%  as.character(), colors = colorRampPalette(BV_colors[2:1])(11)),
      tibble(var = "AMSEL", values =  (0:4) %>%  as.character(), colors = colorRampPalette(BV_colors[2:1])(5)),
    )

  ADEF <-
    ADEF %>%
    select(USUBJID, AVISITN, PARAM, AVALC, DTYPE) %>%
    mutate(
      PARAM2 =
        case_when(
          !is.na(DTYPE) ~ str_c(PARAM, " [",DTYPE,"]"),
          TRUE ~ PARAM
        )
    )



  ADEF_dict <-
    ADEF %>%
    select(PARAM2) %>%
    distinct() %>%
    mutate(
      variable =
        case_when(
          str_detect(PARAM2, "LOCF") ~ "BV_LOCF",
          str_detect(PARAM2, "WC") ~ "BV_WC",
          str_detect(PARAM2, "BV")  ~ "BV",
          str_detect(PARAM2, "ENDPOINT") ~ "SUCCESS_COL_E",
          str_detect(PARAM2, "Successful") ~ "SUCCESS_COL",
          str_detect(PARAM2, "CTV-05") ~ "CTV05",
          str_detect(PARAM2, "crispatus") ~ "LCRISP",
          str_detect(PARAM2, "Lactobacillus") ~ "LACTO",
          str_detect(PARAM2, "Total Bacteria") ~ "LOAD",
          str_detect(PARAM2, "discharge") ~ "DISCH",
          str_detect(PARAM2, "pH") ~ "pH",
          str_detect(PARAM2, "Amine") ~ "AMINE_WT",
          str_detect(PARAM2, "Cells") ~ "CLUE_CELLS",
          str_detect(PARAM2, "Amsel") ~ "AMSEL",
          str_detect(PARAM2, "Nugent") ~ "NUGENT"
        )
    )


  # cat("AMINE_WT", get_fct_values(fct_dict = factor_values, "AMINE_WT"), "\n")
  # cat("DISCH", get_fct_values(fct_dict = factor_values, "DISCH"), "\n")
  # cat("BV", get_fct_values(fct_dict = factor_values, "BV"), "\n")

  BV_diagnosis <-
    ADEF %>%
    left_join(ADEF_dict, by = "PARAM2") %>%
    select(-PARAM, -PARAM2, -DTYPE) %>%
    pivot_wider(
      names_from = variable,
      values_from = AVALC
    ) %>%
    mutate(
      across(
        .cols = starts_with("BV"),
        .fns = function(x)
          factor(x, levels = get_fct_values(fct_dict = factor_values, "BV"))
      ),
      across(
        .cols = starts_with("SUCCESS"),
        .fns = function(x) c(FALSE, TRUE)[(x == "Yes") + 1]
      ),
      across(
        .cols = c(LCRISP, CTV05, LACTO, LOAD, pH),
        .fns = as.double
      ),
      AMINE_WT =
       AMINE_WT %>%
        factor(., get_fct_values(fct_dict = factor_values,"AMINE_WT")),
      DISCH =
       DISCH %>%
        factor(., get_fct_values(fct_dict = factor_values, "DISCH")),
      CLUE_CELLS = CLUE_CELLS %>% as.integer(),
      NUGENT = NUGENT %>% factor(., levels = 0:10),
      AMSEL = AMSEL %>% factor(., levels = 0:4)
    ) %>%
    arrange(USUBJID, AVISITN)

  list(
    BV_diagnosis = BV_diagnosis,
    variable_info = variable_info,
    factor_values = factor_values
  )
}

