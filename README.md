# IIb-LACTIN-V-omics-reanalysis
Analyses (`R` scripts and `Quarto` documents) pertaining to the manuscript "Microbiota effects and predictors of *Lactobacillus crispatus* colonization after treatment with a vaginal live biotherapeutic: results from a randomized, double-blinded, placebo-controlled trial" by Bloom, Symul, et al., 2025. MedRxiv doi: [https://doi.org/10.1101/2025.08.18.25333897](https://www.medrxiv.org/content/10.1101/2025.08.18.25333897v1.article-metrics)



Directories `00`-`02` contain scripts for data cleaning, pre-processing, and assembly into a `Bioconductor` `MultiAssayExperiment` object.

Directory `03` contains sub-directories organized to roughly match the order analyses are presented in the associated manuscript.

`R` functions used across all analysis steps are in the `R/` directory located at the repo root. `R` functions specific to some analyses are located in the corresponding sub-directories.
