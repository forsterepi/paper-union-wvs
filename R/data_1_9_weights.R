# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_9_weights

# Script Description: Create weight variables:
# - d_srh: For dataset used for models with outcome self-rated health
# - d_swl: For dataset used for models with outcome life satisfaction

# d_srh ------------------------------------------------------------------

## Equal contribution for each country within each wave

weights_country_within_wave <- d_srh |>
  dplyr::count(wave, country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    .by = wave
  ) |>
  dplyr::mutate(
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(wave, country, w_cww = w)

## Equal contribution for each country overall

weights_country <- d_srh |>
  dplyr::count(country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(country, w_c = w)

## Equal contribution for each survey

weights_survey <- d_srh |>
  dplyr::count(wave, country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(wave, country, w_s = w)

## Merge

d_srh <- d_srh |>
  dplyr::left_join(
    weights_country_within_wave,
    by = c("country", "wave")
  ) |>
  dplyr::left_join(
    weights_country,
    by = "country"
  ) |>
  dplyr::left_join(
    weights_survey,
    by = c("country", "wave")
  )

rm(weights_country_within_wave, weights_country, weights_survey)

# d_swl ------------------------------------------------------------------

## Equal contribution for each country within each wave

weights_country_within_wave <- d_swl |>
  dplyr::count(wave, country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    .by = wave
  ) |>
  dplyr::mutate(
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(wave, country, w_cww = w)

## Equal contribution for each country overall

weights_country <- d_swl |>
  dplyr::count(country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(country, w_c = w)

## Equal contribution for each survey

weights_survey <- d_swl |>
  dplyr::count(wave, country) |>
  dplyr::mutate(
    prop_obs = n / sum(n),
    prop_pop = 1 / dplyr::n(),
    w = prop_pop / prop_obs,
    weighted_n = round(n * w, 0)
  ) |>
  dplyr::select(wave, country, w_s = w)

## Merge

d_swl <- d_swl |>
  dplyr::left_join(
    weights_country_within_wave,
    by = c("country", "wave")
  ) |>
  dplyr::left_join(
    weights_country,
    by = "country"
  ) |>
  dplyr::left_join(
    weights_survey,
    by = c("country", "wave")
  )

rm(weights_country_within_wave, weights_country, weights_survey)
