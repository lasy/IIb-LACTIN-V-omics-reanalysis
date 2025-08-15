
autofill_date <- function(text){

  j = which(str_length(text) == 4)
  text[j] <- str_c(text[j], "-07-01")
  j = which(str_length(text) == 7)
  text[j] <- str_c(text[j], "-15")

  text <- text %>% as.Date(., format = "%Y-%m-%d")
  text
}
