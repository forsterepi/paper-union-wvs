# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: 0_required-packages

# Script Description: Creates list of required packages. This script is not part
#  of the analysis code, but only creates a data.frame with package names and
#  versions that we used for analysis. This data.frame is saved in this script
#  and loaded in script R/0_init.R for comparision with your available packages.

# Create list of required packages ---------------------------------------

req_pkg <- data.frame(
  pkg = c(
    "rlang",
    "readr",
    "dplyr",
    "magrittr",
    "purrr",
    "storr",
    "haven",
    "Rilostat",
    "vdemdata",
    "ggplot2",
    "patchwork",
    "ordinal",
    "tidyr",
    "plyr",
    "stringi",
    "ggforce",
    "grid",
    "ggrepel",
    "vctrs",
    "flextable",
    "broom",
    "forcats",
    "DiagrammeR",
    "DiagrammeRsvg",
    "rsvg",
    "gtsummary",
    "ggside"
  ),
  version = c(
    "1.3.0",
    "2.2.0",
    "1.2.1",
    "2.0.5",
    "1.2.2",
    "1.2.6",
    "2.5.5",
    "2.5.0",
    "16.0",
    "4.0.3",
    "1.3.2",
    "2026.7-26",
    "1.3.2",
    "1.8.9",
    "1.8.9",
    "0.5.0",
    "4.6.1",
    "0.9.8",
    "0.7.3",
    "0.10.3",
    "1.0.13",
    "1.0.1",
    "1.0.12",
    "0.1",
    "2.7.0",
    "2.6.1",
    "0.4.1"
  )
)

saveRDS(req_pkg, file = file.path("Misc", "required-packages.rds"))
rm(req_pkg)
