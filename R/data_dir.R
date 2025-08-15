
data_dir <- function(){
  dropbox_dir <- "lactinv_dropbox/LACTIN-V Data/"
  if (!dir.exists(dropbox_dir) & str_detect(getwd(), "laurasymul")) {
    dropbox_dir <-  "/Users/laurasymul/Dropbox/Academia/Projects/Gates LACTIN-V/LACTIN-V Data/"
  } else {
    stop("Dropbox directory must be mounted first\n")
  }
  dropbox_dir
}

fig_out_dir <- function(){
  dropbox_dir <- "lactinv_dropbox/LACTIN-V Data/"
  if (!dir.exists(dropbox_dir) & str_detect(getwd(), "laurasymul")) {
    dropbox_dir <-  "/Users/laurasymul/Dropbox/Academia/Projects/Gates LACTIN-V/Manuscript main and supplementary figures/"
  } else {
    stop("Dropbox directory must be mounted first\n")
  }
  dropbox_dir
}