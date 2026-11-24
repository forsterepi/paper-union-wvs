# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_1_load-ictwss

# Script Description: Load and prepare data from ICTWSS database:
#  - Read file from disk
#  - Use UK values for Northern Ireland, as Great Britain and Northern Ireland
#    are treated separately in the World Values Survey
#  - Add the closest value for each year (from the same country). The detailed
#    algorithm is described in the script below.

# Load --------------------------------------------------------------------

if (cache$exists("ictwss")) {
  ictwss <- cache$get("ictwss")
} else {
  # Read file
  ictwss <- readr::read_delim(file.path("Input", "ICTWSS_v2.csv")) |>
    dplyr::select(
      c = iso3,
      country,
      year,
      tud_ictwss = UD_hist,
      cbc_ictwss = AdjCov_hist,
      central_ictwss = Central
    ) |>
    dplyr::mutate(
      central_ictwss = central_ictwss |>
        dplyr::replace_values(
          -99 ~ NA
        ),
      tud_ictwss = tud_ictwss / 100,
      cbc_ictwss = cbc_ictwss / 100
    )

  # Copy UK values for Northern Ireland
  ictwss_nir <- ictwss |>
    dplyr::filter(c == "GBR") |>
    dplyr::mutate(
      c = "NIR"
    )

  ictwss <- rbind(ictwss, ictwss_nir)
  rm(ictwss_nir)

  # Add closest value for each year
  ictwss <- ictwss |>
    dplyr::mutate(
      tud_ictwss_closest = NA,
      cbc_ictwss_closest = NA,
      tud_ictwss_closest_year = NA,
      cbc_ictwss_closest_year = NA
    )

  c_unique <- ictwss$c |> unique()
  out <- vector(mode = "list", length = length(c_unique)) |>
    magrittr::set_names(c_unique)
  rm(c_unique)

  for (i in seq_along(out)) {
    cat(paste0("--- ", names(out)[i], " ---\n"))

    # Subset to country
    temp <- ictwss |>
      dplyr::filter(c == names(out)[i])

    # Subset to available values
    sub_temp_tud <- temp |>
      dplyr::select(year, tud_ictwss) |>
      dplyr::filter(!is.na(tud_ictwss))

    sub_temp_cbc <- temp |>
      dplyr::select(year, cbc_ictwss) |>
      dplyr::filter(!is.na(cbc_ictwss))

    # Loop over temp
    for (j in seq_len(nrow(temp))) {
      # TUD
      if (nrow(sub_temp_tud) > 0) {
        if (!is.na(temp$tud_ictwss[j])) {
          ## If value is available
          temp$tud_ictwss_closest[j] <- temp$tud_ictwss[j]
          temp$tud_ictwss_closest_year[j] <- temp$year[j]
        } else {
          ## Extract closest values
          ## Algorithm:
          ## Step 1: Calculate difference in years for each line
          ## Step 2: Calculate absolute value of difference in years
          ## Step 3: Take smallest absolute value (it's the closest in time)
          ## Step 4: In case there is more than one closest value, take the one
          ##   with the highest year, i.e., the newest one, based on the
          ##   assumption that the statistics tend to get better.
          sub_j_tud <- sub_temp_tud |>
            dplyr::mutate(
              year_diff = temp$year[j] - year, # Step 1
              abs_year_diff = abs(year_diff) # Step 2
            ) |>
            dplyr::filter(abs_year_diff == min(abs_year_diff)) |> # Step 3
            dplyr::filter(year == max(year)) # Step 4

          # Fill in
          temp$tud_ictwss_closest[j] <- sub_j_tud$tud_ictwss[1]
          temp$tud_ictwss_closest_year[j] <- sub_j_tud$year[1]
          rm(sub_j_tud)
        }
      }

      # CBC
      if (nrow(sub_temp_cbc) > 0) {
        if (!is.na(temp$cbc_ictwss[j])) {
          ## If value is available
          temp$cbc_ictwss_closest[j] <- temp$cbc_ictwss[j]
          temp$cbc_ictwss_closest_year[j] <- temp$year[j]
        } else {
          ## Extract closest values (for algorithm, see TUD above)
          sub_j_cbc <- sub_temp_cbc |>
            dplyr::mutate(
              year_diff = temp$year[j] - year, # Step 1
              abs_year_diff = abs(year_diff) # Step 2
            ) |>
            dplyr::filter(abs_year_diff == min(abs_year_diff)) |> # Step 3
            dplyr::filter(year == max(year)) # Step 4

          # Fill in
          temp$cbc_ictwss_closest[j] <- sub_j_cbc$cbc_ictwss[1]
          temp$cbc_ictwss_closest_year[j] <- sub_j_cbc$year[1]
          rm(sub_j_cbc)
        }
      }
    }

    # Fill out
    out[[i]] <- temp
    rm(sub_temp_tud, sub_temp_cbc, temp, j)
  }
  rm(i)

  # List to data.frame
  ictwss <- out |>
    purrr::list_rbind() |>
    as.data.frame()
  rm(out)

  # Add difference in years
  ictwss <- ictwss |>
    dplyr::mutate(
      tud_ictwss_closest_year_diff = year - tud_ictwss_closest_year,
      cbc_ictwss_closest_year_diff = year - cbc_ictwss_closest_year
    )

  # Cache
  cache$set("ictwss", ictwss)
}
