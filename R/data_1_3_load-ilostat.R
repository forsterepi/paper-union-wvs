# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_3_load-ilostat

# Script Description: Load and process ILOSTAT data:
# - Trade union density: Load and process ILOSTAT data on trade union density
# - Collective bargaining coverage: Load and process ILOSTAT data on collective
#   bargaining coverage

# Trade union density -----------------------------------------------------

if (cache$exists("ilostat_tud")) {
  ilostat_tud <- cache$get("ilostat_tud")
} else {
  # Get dictionaries
  dic_notes_indicator <- Rilostat::get_ilostat_dic("note_indicator")
  dic_notes_source <- Rilostat::get_ilostat_dic("note_source")
  dic_ref_area <- Rilostat::get_ilostat_dic("ref_area")
  dic_source <- Rilostat::get_ilostat_dic("source")

  # Prepare dataset
  ilostat_tud <- Rilostat::get_ilostat(
    id = "ILR_TUMT_NOC_RT_A",
    segment = "indicator"
  ) |>
    dplyr::left_join(
      dic_source |>
        dplyr::select(source, source.label),
      by = "source"
    ) |>
    dplyr::left_join(dic_notes_indicator, by = "note_indicator") |>
    dplyr::left_join(dic_notes_source, by = "note_source") |>
    dplyr::left_join(dic_ref_area, by = "ref_area") |>
    dplyr::select(
      c = ref_area,
      country = ref_area.label,
      year = time,
      tud_ilostat = obs_value,
      source,
      source.label,
      note_source,
      note_source.label,
      note_indicator,
      note_indicator.label
    ) |>
    dplyr::mutate(
      year = as.numeric(year),
      tud_ilostat = tud_ilostat / 100
    )
  rm(dic_notes_indicator, dic_notes_source, dic_ref_area, dic_source)

  # Copy UK values for Northern Ireland
  ilostat_tud_nir <- ilostat_tud |>
    dplyr::filter(c == "GBR") |>
    dplyr::mutate(
      c = "NIR"
    )

  ilostat_tud <- rbind(ilostat_tud, ilostat_tud_nir)
  rm(ilostat_tud_nir)

  # Add empty years (1981-2023 because WVS data is from these years)
  tud_add <- expand.grid(unique(ilostat_tud$c), 1981:2023) |>
    magrittr::set_names(c("c", "year")) |>
    dplyr::mutate(
      paste = paste0(c, year)
    ) |>
    dplyr::filter(!(paste %in% paste0(ilostat_tud$c, ilostat_tud$year))) |>
    dplyr::mutate(
      paste = NULL,
      tud_ilostat = NA,
      source = NA,
      source.label = NA,
      note_source = NA,
      note_source.label = NA,
      note_indicator = NA,
      note_indicator.label = NA
    ) |>
    dplyr::arrange(c, year) |>
    dplyr::left_join(
      ilostat_tud |>
        dplyr::select(c, country) |>
        plyr::count() |>
        dplyr::select(c, country),
      by = "c"
    ) |>
    dplyr::select(dplyr::all_of(colnames(ilostat_tud)))

  ilostat_tud <- ilostat_tud |>
    rbind(tud_add) |>
    dplyr::arrange(c, year)
  rm(tud_add)

  # Add closest value for each year
  ilostat_tud <- ilostat_tud |>
    dplyr::mutate(
      tud_ilostat_closest = NA,
      tud_ilostat_closest_year = NA
    )

  c_unique <- ilostat_tud$c |> unique()
  out <- vector(mode = "list", length = length(c_unique)) |>
    magrittr::set_names(c_unique)
  rm(c_unique)

  for (i in seq_along(out)) {
    cat(paste0("--- ", names(out)[i], " ---\n"))

    # Subset to country
    temp <- ilostat_tud |>
      dplyr::filter(c == names(out)[i])

    # Subset to available values
    sub_temp_tud <- temp |>
      dplyr::select(year, tud_ilostat) |>
      dplyr::filter(!is.na(tud_ilostat))

    # Loop over temp
    for (j in seq_len(nrow(temp))) {
      if (nrow(sub_temp_tud) > 0) {
        if (!is.na(temp$tud_ilostat[j])) {
          ## If value is available
          temp$tud_ilostat_closest[j] <- temp$tud_ilostat[j]
          temp$tud_ilostat_closest_year[j] <- temp$year[j]
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
          temp$tud_ilostat_closest[j] <- sub_j_tud$tud_ilostat[1]
          temp$tud_ilostat_closest_year[j] <- sub_j_tud$year[1]
          temp$source[j] <- "closest"
          temp$source.label[j] <- "closest available value"
          rm(sub_j_tud)
        }
      }
    }

    # Fill out
    out[[i]] <- temp
    rm(sub_temp_tud, temp, j)
  }
  rm(i)

  ilostat_tud <- out |>
    purrr::list_rbind() |>
    as.data.frame()
  rm(out)

  ilostat_tud <- ilostat_tud |>
    dplyr::mutate(
      tud_ilostat_closest_year_diff = year - tud_ilostat_closest_year
    )

  # Cache
  cache$set("ilostat_tud", ilostat_tud)
}

# Collective bargaining coverage ------------------------------------------

if (cache$exists("ilostat_cbc")) {
  ilostat_cbc <- cache$get("ilostat_cbc")
} else {
  # Get dictionaries
  dic_notes_indicator <- Rilostat::get_ilostat_dic("note_indicator")
  dic_notes_source <- Rilostat::get_ilostat_dic("note_source")
  dic_ref_area <- Rilostat::get_ilostat_dic("ref_area")
  dic_source <- Rilostat::get_ilostat_dic("source")

  # Prepare dataset
  ilostat_cbc <- Rilostat::get_ilostat(
    id = "ILR_CBCT_NOC_RT_A",
    segment = "indicator"
  ) |>
    dplyr::left_join(
      dic_source |>
        dplyr::select(source, source.label),
      by = "source"
    ) |>
    dplyr::left_join(dic_notes_indicator, by = "note_indicator") |>
    dplyr::left_join(dic_notes_source, by = "note_source") |>
    dplyr::left_join(dic_ref_area, by = "ref_area") |>
    dplyr::select(
      c = ref_area,
      country = ref_area.label,
      year = time,
      cbc_ilostat = obs_value,
      source,
      source.label,
      note_source,
      note_source.label,
      note_indicator,
      note_indicator.label
    ) |>
    dplyr::mutate(
      year = as.numeric(year),
      cbc_ilostat = cbc_ilostat / 100
    )
  rm(dic_notes_indicator, dic_notes_source, dic_ref_area, dic_source)

  # Copy UK values for Northern Ireland
  ilostat_cbc_nir <- ilostat_cbc |>
    dplyr::filter(c == "GBR") |>
    dplyr::mutate(
      c = "NIR"
    )

  ilostat_cbc <- rbind(ilostat_cbc, ilostat_cbc_nir)
  rm(ilostat_cbc_nir)

  # Add empty years
  cbc_add <- expand.grid(unique(ilostat_cbc$c), 1981:2023) |>
    magrittr::set_names(c("c", "year")) |>
    dplyr::mutate(
      paste = paste0(c, year)
    ) |>
    dplyr::filter(!(paste %in% paste0(ilostat_cbc$c, ilostat_cbc$year))) |>
    dplyr::mutate(
      paste = NULL,
      cbc_ilostat = NA,
      source = NA,
      source.label = NA,
      note_source = NA,
      note_source.label = NA,
      note_indicator = NA,
      note_indicator.label = NA
    ) |>
    dplyr::arrange(c, year) |>
    dplyr::left_join(
      ilostat_cbc |>
        dplyr::select(c, country) |>
        plyr::count() |>
        dplyr::select(c, country),
      by = "c"
    ) |>
    dplyr::select(dplyr::all_of(colnames(ilostat_cbc)))

  ilostat_cbc <- ilostat_cbc |>
    rbind(cbc_add) |>
    dplyr::arrange(c, year)
  rm(cbc_add)

  # Add closest value for each year
  ilostat_cbc <- ilostat_cbc |>
    dplyr::mutate(
      cbc_ilostat_closest = NA,
      cbc_ilostat_closest_year = NA
    )

  c_unique <- ilostat_cbc$c |> unique()
  out <- vector(mode = "list", length = length(c_unique)) |>
    magrittr::set_names(c_unique)
  rm(c_unique)

  for (i in seq_along(out)) {
    cat(paste0("--- ", names(out)[i], " ---\n"))

    # Subset to country
    temp <- ilostat_cbc |>
      dplyr::filter(c == names(out)[i])

    # Subset to available values
    sub_temp_cbc <- temp |>
      dplyr::select(year, cbc_ilostat) |>
      dplyr::filter(!is.na(cbc_ilostat))

    # Loop over temp
    for (j in seq_len(nrow(temp))) {
      if (nrow(sub_temp_cbc) > 0) {
        if (!is.na(temp$cbc_ilostat[j])) {
          ## If value is available
          temp$cbc_ilostat_closest[j] <- temp$cbc_ilostat[j]
          temp$cbc_ilostat_closest_year[j] <- temp$year[j]
        } else {
          ## Extract closest values
          ## Algorithm:
          ## Step 1: Calculate difference in years for each line
          ## Step 2: Calculate absolute value of difference in years
          ## Step 3: Take smallest absolute value (it's the closest in time)
          ## Step 4: In case there is more than one closest value, take the one
          ##   with the highest year, i.e., the newest one, based on the
          ##   assumption that the statistics tend to get better.
          sub_j_cbc <- sub_temp_cbc |>
            dplyr::mutate(
              year_diff = temp$year[j] - year, # Step 1
              abs_year_diff = abs(year_diff) # Step 2
            ) |>
            dplyr::filter(abs_year_diff == min(abs_year_diff)) |> # Step 3
            dplyr::filter(year == max(year)) # Step 4

          # Fill in
          temp$cbc_ilostat_closest[j] <- sub_j_cbc$cbc_ilostat[1]
          temp$cbc_ilostat_closest_year[j] <- sub_j_cbc$year[1]
          temp$source[j] <- "closest"
          temp$source.label[j] <- "closest available value"
          rm(sub_j_cbc)
        }
      }
    }

    # Fill out
    out[[i]] <- temp
    rm(sub_temp_cbc, temp, j)
  }
  rm(i)

  ilostat_cbc <- out |>
    purrr::list_rbind() |>
    as.data.frame()
  rm(out)

  ilostat_cbc <- ilostat_cbc |>
    dplyr::mutate(
      cbc_ilostat_closest_year_diff = year - cbc_ilostat_closest_year
    )

  # Cache
  cache$set("ilostat_cbc", ilostat_cbc)
}
