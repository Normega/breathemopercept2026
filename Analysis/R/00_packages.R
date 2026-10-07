# R/00_packages.R
# All package loading for the pipeline. Sourced once by main.R.
# ---------------------------------------------------------------

packages <- c(
  "readxl",
  "tidyverse",
  "ggeffects", "ggforce", "GGally", "ggpubr", "corrplot", "patchwork",
  "psych", "lme4", "lmerTest", "sjPlot",
  "lavaan", "lavaanPlot",
  "signal", "pracma", "reticulate"
)

new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(new_packages)) install.packages(new_packages)

options(readr.show_col_types = FALSE)
for (thispack in packages) {
  library(thispack, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

