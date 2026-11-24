# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_7_merge

# Script Description: Merge data sets:
# - Check data availability: Check if trade union density, collective bargaining
#   coverage, and GINI index are available at some point for a country
# - Subset d_srh: Subset the dataset used for models with outcome self-rated
#   health
# - Subset d_swl: Subset the dataset used for models with outcome life
#   satisfaction
# - Merge ICTWSS and ILOSTAT: Combine the two sources of data on trade union
#   density and collective bargaining coverage
# - Merge TUD/CBC/income with WVS: Merge the individual level data (WVS) with
#   the first country-level datasets (TUD/CBC/income)
# - Merge V-Dem: Add democracy data to main datasets
# - Merge GINI: Add GINI data to main datasets
# - Evaluate availability of centralisation of collective bargaining: Checks the
#   availability of an additional variable that we decided not to use

# Check data availability -------------------------------------------------

## Trade union density
ictwss_tud_countries <- ictwss |>
  dplyr::filter(!is.na(tud_ictwss)) |>
  magrittr::extract2("c") |>
  unique()

ilostat_tud_countries <- ilostat_tud |>
  dplyr::filter(!is.na(tud_ilostat)) |>
  magrittr::extract2("c") |>
  unique()

tud_countries <- c(ictwss_tud_countries, ilostat_tud_countries) |>
  unique()
rm(ictwss_tud_countries, ilostat_tud_countries)

## Collective bargaining coverage
ictwss_cbc_countries <- ictwss |>
  dplyr::filter(!is.na(cbc_ictwss)) |>
  magrittr::extract2("c") |>
  unique()

ilostat_cbc_countries <- ilostat_cbc |>
  dplyr::filter(!is.na(cbc_ilostat)) |>
  magrittr::extract2("c") |>
  unique()

cbc_countries <- c(ictwss_cbc_countries, ilostat_cbc_countries) |>
  unique()
rm(ictwss_cbc_countries, ilostat_cbc_countries)

## GINI
non_gini_countries_srh <- d_srh$c |>
  unique() |>
  magrittr::extract(
    d_srh$c |>
      unique() |>
      magrittr::is_in(gini$c) |>
      magrittr::not()
  )

# Subset d_srh -----------------------------------------------------------

n_50_srh <- nrow(d_srh)

## Exclude countries without any information on trade union density
unique_c_srh <- d_srh$c |>
  unique()

non_tud_wvs_countries_srh <- unique_c_srh |>
  magrittr::extract(!(unique_c_srh %in% tud_countries))

d_srh <- d_srh |>
  dplyr::filter(!(c %in% non_tud_wvs_countries_srh))

n_51_srh <- nrow(d_srh)

## Exclude countries without any information on collective bargaining coverage

non_cbc_wvs_countries_srh <- unique_c_srh |>
  magrittr::extract(!(unique_c_srh %in% cbc_countries))
rm(unique_c_srh)

d_srh <- d_srh |>
  dplyr::filter(!(c %in% non_cbc_wvs_countries_srh))

n_52_srh <- nrow(d_srh)

# Subset d_swl -----------------------------------------------------------

n_50_swl <- nrow(d_swl)

## Exclude countries without any information on trade union density
unique_c_swl <- d_swl$c |>
  unique()

non_tud_wvs_countries_swl <- unique_c_swl |>
  magrittr::extract(!(unique_c_swl %in% tud_countries))

d_swl <- d_swl |>
  dplyr::filter(!(c %in% non_tud_wvs_countries_swl))

n_51_swl <- nrow(d_swl)

## Exclude countries without any information on collective bargaining coverage
non_cbc_wvs_countries_swl <- unique_c_swl |>
  magrittr::extract(!(unique_c_swl %in% cbc_countries))
rm(unique_c_swl)

d_swl <- d_swl |>
  dplyr::filter(!(c %in% non_cbc_wvs_countries_swl))

n_52_swl <- nrow(d_swl)

rm(cbc_countries)
rm(tud_countries)

# Merge ICTWSS and ILOSTAT ------------------------------------------------

## TUD
### Combine datasets
tud <- ictwss |>
  dplyr::select(
    c,
    country_ictwss = country,
    year,
    tud_ictwss,
    tud_ictwss_closest,
    tud_ictwss_closest_year,
    tud_ictwss_closest_year_diff
  ) |>
  dplyr::full_join(
    ilostat_tud |>
      dplyr::select(
        c,
        country_ilostat = country,
        year,
        tud_ilostat,
        tud_ilostat_closest,
        tud_ilostat_closest_year,
        tud_ilostat_closest_year_diff
      ),
    by = c("c", "year")
  ) |>
  dplyr::mutate(
    tud = NA,
    tud_year = NA,
    tud_source = NA
  )

### Choose ilostat or ictwss
for (i in seq_len(nrow(tud))) {
  if (is.na(tud$tud_ictwss_closest[i])) {
    tud$tud[i] <- tud$tud_ilostat_closest[i]
    tud$tud_year[i] <- tud$tud_ilostat_closest_year[i]
    tud$tud_source[i] <- "ILOSTAT"
  }
  if (is.na(tud$tud_ilostat_closest[i])) {
    tud$tud[i] <- tud$tud_ictwss_closest[i]
    tud$tud_year[i] <- tud$tud_ictwss_closest_year[i]
    tud$tud_source[i] <- "ICTWSS"
  }
  if (!is.na(tud$tud_ictwss_closest[i]) && !is.na(tud$tud_ilostat_closest[i])) {
    if (
      abs(tud$tud_ictwss_closest_year_diff[i]) <=
        abs(tud$tud_ilostat_closest_year_diff[i])
    ) {
      tud$tud[i] <- tud$tud_ictwss_closest[i]
      tud$tud_year[i] <- tud$tud_ictwss_closest_year[i]
      tud$tud_source[i] <- "ICTWSS"
    }
    if (
      abs(tud$tud_ictwss_closest_year_diff[i]) >
        abs(tud$tud_ilostat_closest_year_diff[i])
    ) {
      tud$tud[i] <- tud$tud_ilostat_closest[i]
      tud$tud_year[i] <- tud$tud_ilostat_closest_year[i]
      tud$tud_source[i] <- "ILOSTAT"
    }
  }
}
rm(i)

### Post-processing
tud <- tud |>
  dplyr::mutate(
    tud_year_diff = year - tud_year
  ) |>
  dplyr::select(c, year, tud, tud_year, tud_year_diff, tud_source) |>
  dplyr::filter(!is.na(tud))

## CBC
### Combine datasets
cbc <- ictwss |>
  dplyr::select(
    c,
    country_ictwss = country,
    year,
    cbc_ictwss,
    cbc_ictwss_closest,
    cbc_ictwss_closest_year,
    cbc_ictwss_closest_year_diff
  ) |>
  dplyr::full_join(
    ilostat_cbc |>
      dplyr::select(
        c,
        country_ilostat = country,
        year,
        cbc_ilostat,
        cbc_ilostat_closest,
        cbc_ilostat_closest_year,
        cbc_ilostat_closest_year_diff
      ),
    by = c("c", "year")
  ) |>
  dplyr::mutate(
    cbc = NA,
    cbc_year = NA,
    cbc_source = NA
  )

### Choose ilostat or ictwss
for (i in seq_len(nrow(cbc))) {
  if (is.na(cbc$cbc_ictwss_closest[i])) {
    cbc$cbc[i] <- cbc$cbc_ilostat_closest[i]
    cbc$cbc_year[i] <- cbc$cbc_ilostat_closest_year[i]
    cbc$cbc_source[i] <- "ILOSTAT"
  }
  if (is.na(cbc$cbc_ilostat_closest[i])) {
    cbc$cbc[i] <- cbc$cbc_ictwss_closest[i]
    cbc$cbc_year[i] <- cbc$cbc_ictwss_closest_year[i]
    cbc$cbc_source[i] <- "ICTWSS"
  }
  if (!is.na(cbc$cbc_ictwss_closest[i]) && !is.na(cbc$cbc_ilostat_closest[i])) {
    if (
      abs(cbc$cbc_ictwss_closest_year_diff[i]) <=
        abs(cbc$cbc_ilostat_closest_year_diff[i])
    ) {
      cbc$cbc[i] <- cbc$cbc_ictwss_closest[i]
      cbc$cbc_year[i] <- cbc$cbc_ictwss_closest_year[i]
      cbc$cbc_source[i] <- "ICTWSS"
    }
    if (
      abs(cbc$cbc_ictwss_closest_year_diff[i]) >
        abs(cbc$cbc_ilostat_closest_year_diff[i])
    ) {
      cbc$cbc[i] <- cbc$cbc_ilostat_closest[i]
      cbc$cbc_year[i] <- cbc$cbc_ilostat_closest_year[i]
      cbc$cbc_source[i] <- "ILOSTAT"
    }
  }
}
rm(i)

### Post-processing
cbc <- cbc |>
  dplyr::mutate(
    cbc_year_diff = year - cbc_year
  ) |>
  dplyr::select(c, year, cbc, cbc_year, cbc_year_diff, cbc_source) |>
  dplyr::filter(!is.na(cbc))

## Clean
rm(ilostat_tud, ilostat_cbc)

# Merge TUD/CBC/income with WVS  ------------------------------------------

d_srh <- d_srh |>
  dplyr::left_join(
    country_classification |>
      dplyr::select(-country),
    by = c("c", "year")
  ) |>
  dplyr::left_join(tud, by = c("c", "year")) |>
  dplyr::left_join(cbc, by = c("c", "year"))

d_swl <- d_swl |>
  dplyr::left_join(
    country_classification |>
      dplyr::select(-country),
    by = c("c", "year")
  ) |>
  dplyr::left_join(tud, by = c("c", "year")) |>
  dplyr::left_join(cbc, by = c("c", "year"))

# Merge V-Dem -------------------------------------------------------------

d_srh <- d_srh |>
  dplyr::left_join(vdem, by = c("c", "year"))

d_swl <- d_swl |>
  dplyr::left_join(vdem, by = c("c", "year"))
rm(vdem)

# Merge GINI -------------------------------------------------------------

d_srh <- d_srh |>
  dplyr::left_join(gini, by = c("c", "year"))

d_swl <- d_swl |>
  dplyr::left_join(gini, by = c("c", "year"))
rm(gini)

# Evaluate availability of centralisation of collective bargaining --------

ictwss_cent_countries <- ictwss |>
  dplyr::filter(!is.na(central_ictwss)) |>
  magrittr::extract2("c") |>
  unique()

cent_wvs_countries_srh <- d_srh |>
  dplyr::select(c, country) |>
  dplyr::filter(c %in% ictwss_cent_countries) |>
  magrittr::extract2("country") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

non_cent_wvs_countries_srh <- d_srh |>
  dplyr::select(c, country) |>
  dplyr::filter(!(c %in% ictwss_cent_countries)) |>
  magrittr::extract2("country") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

cent_wvs_countries_swl <- d_swl |>
  dplyr::select(c, country) |>
  dplyr::filter(c %in% ictwss_cent_countries) |>
  magrittr::extract2("country") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

non_cent_wvs_countries_swl <- d_swl |>
  dplyr::select(c, country) |>
  dplyr::filter(!(c %in% ictwss_cent_countries)) |>
  magrittr::extract2("country") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

rm(ictwss, ictwss_cent_countries)
