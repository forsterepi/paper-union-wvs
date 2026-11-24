# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_5_load-vdem

# Script Description: Load and process V-Dem data on democracy:
#  - Load: Load data using the `vdemdata` package, select necessary variables,
#    and filter the data
#  - Repeat GBR values for NIR: Use values for Great Britain for Northern
#    Ireland as they are treated separately by the World Values Survey
#  - Extend Montenegro: Use Serbian values for Montenegro before 1998

# Load --------------------------------------------------------------------

vdem <- vdemdata::vdem |>
  dplyr::select(
    country_name,
    c = country_text_id,
    year,
    v2x_libdem,
    v2x_libdem_codelow,
    v2x_libdem_codehigh,
    v2x_libdem_sd
  ) |>
  dplyr::filter(year > 1980)

# Repeat GBR values for NIR -----------------------------------------------

## UK, i.e., GBR values apply to Northern Ireland

vdem_nir <- vdem |>
  dplyr::filter(c == "GBR") |>
  dplyr::mutate(
    c = "NIR"
  )

vdem <- vdem |> rbind(vdem_nir)
rm(vdem_nir)

# Extend Montenegro -------------------------------------------------------

## 1998 is the first year with separate values for Montenegro
## Before, values listed under Serbia apply

vdem_mne <- vdem |>
  dplyr::filter(c == "SRB", year < 1998) |>
  dplyr::mutate(
    c = "MNE",
    country_name = "Montenegro"
  )

vdem <- vdem |>
  rbind(vdem_mne) |>
  dplyr::arrange(c, year)
rm(vdem_mne)
