# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_8_finalise

# Script Description: Finalise data preparation after merging:
# - Additional processing: Additionally filtering out rows in the main datasets
# - Standardise continuous variables: Calculating standardised versions of
#   continuous variables

# Additional processing --------------------------------------------------

## srh
d_srh <- d_srh |>
  # Only keep individuals with employment status full time, part time, or
  # self-employed
  dplyr::filter(empl_stat %in% c("Full time", "Part time", "Self employed")) |>
  # Transform country to factor
  dplyr::mutate(
    country = forcats::fct_drop(country)
  ) |>
  # Transform wave to factor
  dplyr::mutate(
    wave_num = wave,
    wave = as.factor(wave),
    wave_relevel = relevel(wave, ref = "4")
  )

n_60_srh <- nrow(d_srh)

d_srh_covid <- d_srh
d_srh <- d_srh |>
  # Exclude covid (wave 7)
  dplyr::filter(year < 2020)

n_61_srh <- nrow(d_srh)

d_srh <- d_srh |>
  # Exclude the Dominican Republic because noone there has poor or very
  # poor self-rated health
  dplyr::filter(country != "Dominican Republic")

n_62_srh <- nrow(d_srh)

## swl
d_swl <- d_swl |>
  # Only keep individuals with employment status full time, part time, or
  # self-employed
  dplyr::filter(empl_stat %in% c("Full time", "Part time", "Self employed")) |>
  # Transform country to factor
  dplyr::mutate(
    country = forcats::fct_drop(country)
  ) |>
  # Transform wave to factor
  dplyr::mutate(
    wave_num = wave,
    wave = as.factor(wave),
    wave_relevel = relevel(wave, ref = "4")
  )

n_60_swl <- nrow(d_swl)

d_swl_covid <- d_swl
d_swl <- d_swl |>
  # Exclude covid (wave 7)
  dplyr::filter(year < 2020)

n_61_swl <- nrow(d_swl)

# Standardise continuous variables ---------------------------------------

## Get mean and SD for reporting
tud_mean_srh <- mean(d_srh$tud)
cbc_mean_srh <- mean(d_srh$cbc)
libdem_mean_srh <- mean(d_srh$v2x_libdem)
gini_mean_srh <- mean(d_srh$gini, na.rm = TRUE)

tud_sd_srh <- sd(d_srh$tud)
cbc_sd_srh <- sd(d_srh$cbc)
libdem_sd_srh <- sd(d_srh$v2x_libdem)
gini_sd_srh <- sd(d_srh$gini, na.rm = TRUE)

tud_sd_perc_srh <- sd(d_srh$tud) |>
  magrittr::multiply_by(100) |>
  round(1) |>
  format(nsmall = 1) |>
  paste0("%")
cbc_sd_perc_srh <- sd(d_srh$cbc) |>
  magrittr::multiply_by(100) |>
  round(1) |>
  format(nsmall = 1) |>
  paste0("%")

tud_mean_swl <- mean(d_swl$tud)
cbc_mean_swl <- mean(d_swl$cbc)
libdem_mean_swl <- mean(d_swl$v2x_libdem)
gini_mean_swl <- mean(d_swl$gini, na.rm = TRUE)

tud_sd_swl <- sd(d_swl$tud)
cbc_sd_swl <- sd(d_swl$cbc)
libdem_sd_swl <- sd(d_swl$v2x_libdem)
gini_sd_swl <- sd(d_swl$gini, na.rm = TRUE)

tud_sd_perc_swl <- sd(d_swl$tud) |>
  magrittr::multiply_by(100) |>
  round(1) |>
  format(nsmall = 1) |>
  paste0("%")
cbc_sd_perc_swl <- sd(d_swl$cbc) |>
  magrittr::multiply_by(100) |>
  round(1) |>
  format(nsmall = 1) |>
  paste0("%")

# Do standardisation
d_srh <- d_srh |>
  dplyr::mutate(
    tud_original = tud,
    cbc_original = cbc,
    v2x_libdem_original = v2x_libdem,
    gini_original = gini,
    tud = (tud - mean(tud)) / sd(tud),
    cbc = (cbc - mean(cbc)) / sd(cbc),
    v2x_libdem = (v2x_libdem - mean(v2x_libdem)) / sd(v2x_libdem),
    gini = (gini - mean(gini, na.rm = TRUE)) / sd(gini, na.rm = TRUE)
  ) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    tud_c_cen100 = (tud_original - mean(tud_original)) * 100,
    cbc_c_cen100 = (cbc_original - mean(cbc_original)) * 100
  ) |>
  dplyr::ungroup()

d_swl <- d_swl |>
  dplyr::mutate(
    tud_original = tud,
    cbc_original = cbc,
    v2x_libdem_original = v2x_libdem,
    gini_original = gini,
    tud = (tud - mean(tud)) / sd(tud),
    cbc = (cbc - mean(cbc)) / sd(cbc),
    v2x_libdem = (v2x_libdem - mean(v2x_libdem)) / sd(v2x_libdem),
    gini = (gini - mean(gini, na.rm = TRUE)) / sd(gini, na.rm = TRUE)
  ) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    tud_c_cen100 = (tud_original - mean(tud_original)) * 100,
    cbc_c_cen100 = (cbc_original - mean(cbc_original)) * 100
  ) |>
  dplyr::ungroup()
