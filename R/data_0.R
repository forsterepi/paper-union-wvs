# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: data_0

# Script Description: Runs all data processing scripts and creates the postdata
#  object

# Prepare data -----------------------------------------------------------

if (cache$exists("postdata")) {
  postdata <- cache$get("postdata")

  d_srh <- postdata$d_srh
  d_swl <- postdata$d_swl
  n_0 <- postdata$n_0
  n_2 <- postdata$n_2
  n_3 <- postdata$n_3
  n_4 <- postdata$n_4
  n_5 <- postdata$n_5
  n_10_srh <- postdata$n_10_srh
  n_10_swl <- postdata$n_10_swl
  n_50_srh <- postdata$n_50_srh
  n_51_srh <- postdata$n_51_srh
  n_52_srh <- postdata$n_52_srh
  n_50_swl <- postdata$n_50_swl
  n_51_swl <- postdata$n_51_swl
  n_52_swl <- postdata$n_52_swl
  n_60_srh <- postdata$n_60_srh
  n_60_swl <- postdata$n_60_swl
  n_61_srh <- postdata$n_61_srh
  n_61_swl <- postdata$n_61_swl
  n_62_srh <- postdata$n_62_srh
  cwave_excluded_2 <- postdata$cwave_excluded_2
  cwave_excluded_3 <- postdata$cwave_excluded_3
  cwave_excluded_4 <- postdata$cwave_excluded_4
  cwave_excluded_5 <- postdata$cwave_excluded_5
  cwave_excluded_10_srh <- postdata$cwave_excluded_10_srh
  cwave_excluded_10_swl <- postdata$cwave_excluded_10_swl
  non_tud_wvs_countries_srh <- postdata$non_tud_wvs_countries_srh
  non_cbc_wvs_countries_srh <- postdata$non_cbc_wvs_countries_srh
  cent_wvs_countries_srh <- postdata$cent_wvs_countries_srh
  non_cent_wvs_countries_srh <- postdata$non_cent_wvs_countries_srh
  non_tud_wvs_countries_swl <- postdata$non_tud_wvs_countries_swl
  non_cbc_wvs_countries_swl <- postdata$non_cbc_wvs_countries_swl
  cent_wvs_countries_swl <- postdata$cent_wvs_countries_swl
  non_cent_wvs_countries_swl <- postdata$non_cent_wvs_countries_swl
  country_classification <- postdata$country_classification
  tud <- postdata$tud
  cbc <- postdata$cbc
  cbc_mean_srh <- postdata$cbc_mean_srh
  cbc_sd_srh <- postdata$cbc_sd_srh
  cbc_sd_perc_srh <- postdata$cbc_sd_perc_srh
  libdem_mean_srh <- postdata$libdem_mean_srh
  libdem_sd_srh <- postdata$libdem_sd_srh
  tud_mean_srh <- postdata$tud_mean_srh
  tud_sd_srh <- postdata$tud_sd_srh
  tud_sd_perc_srh <- postdata$tud_sd_perc_srh
  cbc_mean_swl <- postdata$cbc_mean_swl
  cbc_sd_swl <- postdata$cbc_sd_swl
  cbc_sd_perc_swl <- postdata$cbc_sd_perc_swl
  libdem_mean_swl <- postdata$libdem_mean_swl
  libdem_sd_swl <- postdata$libdem_sd_swl
  tud_mean_swl <- postdata$tud_mean_swl
  tud_sd_swl <- postdata$tud_sd_swl
  tud_sd_perc_swl <- postdata$tud_sd_perc_swl
  gini_mean_srh <- postdata$gini_mean_srh
  gini_sd_srh <- postdata$gini_sd_srh
  gini_mean_swl <- postdata$gini_mean_swl
  gini_sd_swl <- postdata$gini_sd_swl
  non_gini_countries_srh <- postdata$non_gini_countries_srh
  d_srh_covid <- postdata$d_srh_covid
  d_swl_covid <- postdata$d_swl_covid
} else {
  ## Load and prepare data from ICTWSS database
  source(file.path("R", "data_1_1_load-ictwss.R"))

  ## Loading world value survey data
  source(file.path("R", "data_1_2_load-wvs.R"))

  ## Load and process ILOSTAT data
  source(file.path("R", "data_1_3_load-ilostat.R"))

  ## Load World Bank classification into high, upper middle,
  ## lower middle, and low income countries
  source(file.path("R", "data_1_4_load-country-classification.R"))

  ## Load and process V-Dem data on democracy
  source(file.path("R", "data_1_5_load-vdem.R"))

  ## Load and process GINI index data
  source(file.path("R", "data_1_6_load-gini.R"))

  ## Merge data sets
  source(file.path("R", "data_1_7_merge.R"))

  ## Finalise
  source(file.path("R", "data_1_8_finalise.R"))

  ## Weights
  source(file.path("R", "data_1_9_weights.R"))

  ## Cache
  postdata <- list(
    d_srh = d_srh,
    d_swl = d_swl,
    n_0 = n_0,
    n_2 = n_2,
    n_3 = n_3,
    n_4 = n_4,
    n_5 = n_5,
    n_10_srh = n_10_srh,
    n_10_swl = n_10_swl,
    n_50_srh = n_50_srh,
    n_51_srh = n_51_srh,
    n_52_srh = n_52_srh,
    n_50_swl = n_50_swl,
    n_51_swl = n_51_swl,
    n_52_swl = n_52_swl,
    n_60_srh = n_60_srh,
    n_60_swl = n_60_swl,
    n_61_srh = n_61_srh,
    n_61_swl = n_61_swl,
    n_62_srh = n_62_srh,
    cwave_excluded_2 = cwave_excluded_2,
    cwave_excluded_3 = cwave_excluded_3,
    cwave_excluded_4 = cwave_excluded_4,
    cwave_excluded_5 = cwave_excluded_5,
    cwave_excluded_10_srh = cwave_excluded_10_srh,
    cwave_excluded_10_swl = cwave_excluded_10_swl,
    non_tud_wvs_countries_srh = non_tud_wvs_countries_srh,
    non_cbc_wvs_countries_srh = non_cbc_wvs_countries_srh,
    cent_wvs_countries_srh = cent_wvs_countries_srh,
    non_cent_wvs_countries_srh = non_cent_wvs_countries_srh,
    non_tud_wvs_countries_swl = non_tud_wvs_countries_swl,
    non_cbc_wvs_countries_swl = non_cbc_wvs_countries_swl,
    cent_wvs_countries_swl = cent_wvs_countries_swl,
    non_cent_wvs_countries_swl = non_cent_wvs_countries_swl,
    country_classification = country_classification,
    tud = tud,
    cbc = cbc,
    cbc_mean_srh = cbc_mean_srh,
    cbc_sd_srh = cbc_sd_srh,
    cbc_sd_perc_srh = cbc_sd_perc_srh,
    libdem_mean_srh = libdem_mean_srh,
    libdem_sd_srh = libdem_sd_srh,
    tud_mean_srh = tud_mean_srh,
    tud_sd_srh = tud_sd_srh,
    tud_sd_perc_srh = tud_sd_perc_srh,
    cbc_mean_swl = cbc_mean_swl,
    cbc_sd_swl = cbc_sd_swl,
    cbc_sd_perc_swl = cbc_sd_perc_swl,
    libdem_mean_swl = libdem_mean_swl,
    libdem_sd_swl = libdem_sd_swl,
    tud_mean_swl = tud_mean_swl,
    tud_sd_swl = tud_sd_swl,
    tud_sd_perc_swl = tud_sd_perc_swl,
    gini_mean_srh = gini_mean_srh,
    gini_sd_srh = gini_sd_srh,
    gini_mean_swl = gini_mean_swl,
    gini_sd_swl = gini_sd_swl,
    non_gini_countries_srh = non_gini_countries_srh,
    d_srh_covid = d_srh_covid,
    d_swl_covid = d_swl_covid
  )

  cache$set("postdata", postdata)
}
rm(postdata)
