test_topic_comp_by_ARM <- function(gammas, visit = 0){

  visit_data <-
    gammas %>%
    filter(AVISITN == visit, !is.na(ARM), EOSSTT == "COMPLETED") %>%
    select(Barcode, topic_label, prop, ARM)

  visit_data_w <-
    visit_data %>%
    tidyr::pivot_wider(
      id_cols = c(Barcode, ARM),
      names_from = topic_label, values_from = prop)  %>%
    select(-Barcode)


  y <- visit_data_w %>% select(-ARM) %>% as.matrix()
  x <- visit_data_w %>% select(ARM) %>% as.data.frame()
  data <- x
  data$y <- DirichletReg::DR_data(y)

  dirichlet <- DirichletReg::DirichReg(y ~ ARM, data = data)
  null <- DirichletReg::DirichReg(y ~ 1, data = data)
  list(full = dirichlet, null = null)
}
