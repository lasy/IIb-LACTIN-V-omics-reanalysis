
get_swab_info <- function(dropbox_dir) {

  # GT : Global trace
  # contains info for ALL swabs collected
  GT <-
    readxl::read_xlsx(
      str_c(
        dropbox_dir,
        "00_raw/sample_inventory/",
        "2. Global Trace samples - incl site ID, collection date, visit number.xlsx"
      ),
      skip = 1,
      guess_max = 10000
    )
  
  # contains list of swabs that were shipped to Doug/Seth/...
  swabs_shipped <-
    readxl::read_xlsx(
      str_c(
        dropbox_dir,
        "00_raw/sample_inventory/",
        "LactinV_Dec02_2020_specimeninventory from Maira 09.2021.xlsx"
      ),
      guess_max = 10000
    )
  
  

  # The format for the participant ID and visit number is not exactly the same as in the clinical database so we need to make some adjustments:

  GT <-
    GT %>%
    left_join(
      ADSL %>% select(USUBJID, SUBJID) %>% rename(PATID = SUBJID),
      by = "PATID"
    ) %>%
    mutate(
      VISNO_round = VISNO %>% str_sub(1,2) %>% as.integer(),
      VISNO_suppl = VISNO %>% str_sub(3,3),
      VISNO_suppl_n =
        case_when(
          VISNO_suppl == "S" ~ 0.19,
          VISNO_suppl == "T" ~ 0.2,
          TRUE ~ 0
        ),
      AVISITN = VISNO_round + VISNO_suppl_n
    )  %>%
    select(-starts_with("VISNO_"))

  swabs_shipped <-
    swabs_shipped %>%
    select(Barcode) %>%
    left_join(
      GT %>%
        select(USUBJID, AVISITN, SN) %>%
        rename(Barcode = SN) %>%
        mutate(Barcode = Barcode %>% as.numeric()),
      by = "Barcode"
    )

  list(
    all_swabs = GT,
    shipped_swabs = swabs_shipped
  )

}
