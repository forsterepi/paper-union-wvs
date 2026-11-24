# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: run-project

# Script Description: Run this file to conduct the analysis

# Initialisation ---------------------------------------------------------

source(file.path("R", "0_init.R"))

# Data -------------------------------------------------------------------

## Data processing ----
source(file.path("R", "data_0.R"))

## Descriptives ----
source(file.path("R", "data_2_descriptives.R"))

# Post-data --------------------------------------------------------------

## Fit main models ----
source(file.path("R", "post_1_1_srhtud.R"))
source(file.path("R", "post_1_2_swltud.R"))
source(file.path("R", "post_1_3_srhcbc.R"))
source(file.path("R", "post_1_4_swlcbc.R"))

## Create model-based figures for main text ----
source(file.path("R", "post_2_1_figures.R"))
source(file.path("R", "post_2_2_pred-lower-middle.R"))

## Sensitivity analyses ----
source(file.path("R", "post_3_1_sens-tud-cbc-available.R"))
source(file.path("R", "post_3_2_sens-with-covid.R"))
source(file.path("R", "post_3_3_sens-weighted.R"))
source(file.path("R", "post_3_4_sens-by-employment-status.R"))
source(file.path("R", "post_3_5_sens-no-interaction_srhtud.R"))
source(file.path("R", "post_3_6_sens-no-interaction_swltud.R"))
source(file.path("R", "post_3_7_sens-no-interaction_srhcbc.R"))
source(file.path("R", "post_3_8_sens-no-interaction_swlcbc.R"))
