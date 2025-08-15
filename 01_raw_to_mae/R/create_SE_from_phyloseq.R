


#' Create a SummarizedExperiment object from a phyloseq object
#'
#' @param phyloseq_RDS_file
#'
#' @return a `SummarizedExperiment` object
#' @export
#'
create_SE_from_phyloseq <- function(phyloseq_object){

  if (
    ("Kingdom" %in% colnames(phyloseq_object@tax_table)) &
    !("Domain" %in% colnames(phyloseq_object@tax_table))
    ) {
    j <- which(colnames(phyloseq_object@tax_table) == "Kingdom")
    colnames(phyloseq_object@tax_table)[j] <- "Domain"
  }



  tax_table <-
    phyloseq_object@tax_table %>%
    as.data.frame() %>%
    mutate(
      sequence = rownames(phyloseq_object@tax_table),
      nb = row_number(),
      tax_label =
        case_when(
          !(Species == "") & !(Genus == "") ~ paste0(Genus, " ", Species),
          (Species == "") & !(Genus == "unassigned") ~ paste0(Genus, " (g)"),
          (Genus == "unassigned") & !(Family == "unassigned") ~ paste0(Family, " (f)"),
          (Family == "unassigned") & !(Order == "unassigned") ~ paste0(Order, " (o)"),
          (Order == "unassigned") & !(Class == "unassigned") ~ paste0(Class, " (c)"),
          (Class == "unassigned") & !(Phylum == "unassigned") ~ paste0(Phylum, " (p)"),
          (Phylum == "unassigned") & !(Domain == "unassigned") ~ paste0(Domain, " (d)"),
          TRUE ~ "unassigned"
        )
    )

  if (any(duplicated(tax_table$tax_label))) {
    tax_table <-  tax_table %>% mutate(key = paste0("|# ", nb, "| ", tax_label))
  } else {
    tax_table$key <- tax_table$tax_label
  }

  rownames(tax_table) <- tax_table$key

  assay_mat <-
    phyloseq_object@otu_table %>%
    as.matrix() %>%
    t() %>%  # samples must be on the columns for MAE
    set_rownames(rownames(tax_table))


  if (! ("SampleID" %in% colnames(phyloseq_object@sam_data)))
    phyloseq_object@sam_data$Sample_ID <- rownames(phyloseq_object@sam_data)

  assay_colDat <-
    phyloseq_object@sam_data %>%
    as_tibble() %>%
    mutate(Barcode = phyloseq_object@sam_data$Sample_ID) %>%
    as.data.frame()

  SE <-
    SummarizedExperiment::SummarizedExperiment(
      assays = assay_mat,
      colData = assay_colDat,
      rowData = tax_table
    )

  SE
}
