# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_6_load-gini

# Script Description: Load and process World Bank GINI data:
# - Read file from disk
# - Use UK values for Northern Ireland, as Great Britain and Northern Ireland
#   are treated separately in the World Values Survey
# - Add Serbina values to Montenegro
# - Add the closest value for each year (from the same country). The detailed
#   algorithm is described in the script below.

# Load -------------------------------------------------------------------

if (cache$exists("gini")) {
  gini <- cache$get("gini")
} else {
  gini <- readr::read_delim(
    file = file.path("Input", "gini", "API_SI.POV.GINI.csv")
  ) |>
    dplyr::select(
      c_name = `Country Name`,
      c = `Country Code`,
      `1960`:`2025`
    )

  meta <- readr::read_delim(
    file = file.path(
      "Input",
      "gini",
      "Metadata_Country_API_SI.POV.GINI.csv"
    )
  ) |>
    dplyr::select(c = `Country Code`, gini_note = SpecialNotes)

  # Merge
  gini <- gini |>
    dplyr::left_join(meta, by = "c") |>
    dplyr::relocate(gini_note, .after = c)
  rm(meta)

  # Copy UK values for Northern Ireland
  gini_nir <- gini |>
    dplyr::filter(c == "GBR") |>
    dplyr::mutate(
      c = "NIR",
      c_name = "Northern Ireland"
    )

  gini <- vctrs::vec_rbind(gini, gini_nir)
  rm(gini_nir)

  # Add Serbian values to Montenegro
  ## The first year in which a value is available for Montenegro is 2005.
  ## The first year in which a value is available for Serbia is 2002.
  ## We use the Serbian values for 2002, 2003, and 2004 for Montenegro.
  ## No historical data for Yugoslavia or the Soviet Union are available.
  ## We cannot make the other adjustments for SRB, MNE, RUS, that we made for
  ## country classifications by income.

  gini$`2002`[gini$c == "MNE"] <- gini$`2002`[gini$c == "SRB"]
  gini$`2003`[gini$c == "MNE"] <- gini$`2003`[gini$c == "SRB"]
  gini$`2004`[gini$c == "MNE"] <- gini$`2004`[gini$c == "SRB"]

  # Turn to long format
  gini <- gini |>
    tidyr::pivot_longer(
      cols = `1960`:`2025`,
      names_to = "year",
      values_to = "gini"
    ) |>
    dplyr::mutate(
      year = as.numeric(year)
    )

  # Add closest value for each year
  gini <- gini |>
    dplyr::mutate(
      gini_closest = NA,
      gini_closest_year = NA
    )

  c_unique <- gini$c |>
    unique()
  out <- vector(mode = "list", length = length(c_unique)) |>
    magrittr::set_names(c_unique)
  rm(c_unique)

  for (i in seq_along(out)) {
    cat(paste0("--- ", names(out)[i], " ---\n"))

    # Subset to country
    temp <- gini |>
      dplyr::filter(c == names(out)[i])

    # Subset to available values
    sub_temp <- temp |>
      dplyr::select(year, gini) |>
      dplyr::filter(!is.na(gini))

    # Loop over temp
    for (j in seq_len(nrow(temp))) {
      if (nrow(sub_temp) > 0) {
        if (!is.na(temp$gini[j])) {
          ## If value is available
          temp$gini_closest[j] <- temp$gini[j]
          temp$gini_closest_year[j] <- temp$year[j]
        } else {
          ## Extract closest values
          ## Algorithm:
          ## Step 1: Calculate difference in years for each line
          ## Step 2: Calculate absolute value of difference in years
          ## Step 3: Take smallest absolute value (it's the closest in time)
          ## Step 4: In case there is more than one closest value, take the one
          ##   with the highest year, i.e., the newest one, based on the
          ##   assumption that the statistics tend to get better.
          sub_j <- sub_temp |>
            dplyr::mutate(
              year_diff = temp$year[j] - year, # Step 1
              abs_year_diff = abs(year_diff) # Step 2
            ) |>
            dplyr::filter(abs_year_diff == min(abs_year_diff)) |> # Step 3
            dplyr::filter(year == max(year)) # Step 4

          # Fill in
          temp$gini_closest[j] <- sub_j$gini[1]
          temp$gini_closest_year[j] <- sub_j$year[1]
          rm(sub_j)
        }
      }
    }

    # Fill out
    out[[i]] <- temp
    rm(sub_temp, temp, j)
  }
  rm(i)

  # List to data.frame
  gini <- out |>
    purrr::list_rbind()
  rm(out)

  ## Remove countries/regions without any values
  gini <- gini |>
    dplyr::filter_out(is.na(gini_closest))

  # Add difference in years and subset
  gini <- gini |>
    dplyr::mutate(
      gini_closest_year_diff = year - gini_closest_year
    ) |>
    dplyr::select(
      c,
      year,
      gini = gini_closest,
      gini_year = gini_closest_year,
      gini_closest_year_diff = gini_closest_year_diff,
      gini_note
    )

  # Cache
  cache$set("gini", gini)
}
