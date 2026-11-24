# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_3_7_sens-no-interaction_srhcbc

# Script Description: Sensitivity analysis: Re-fit fully adjusted models
#  stratified by wave instead of the interaction term. This script creates
#  Figure S8c.
# - Define variables: Create plot title
# - Create data sets: Create stratified datasets
# - Fit models: Fit stratified models and extract estimates (functions in the
#   function package can only be used fot models that contain the interaction)
# - Estimates from primary model: Extract estimates from the interaction model
#   for comparison
# - Plot: Create Figure S8c

# Define variables -------------------------------------------------------

plot_title <- "CBC - Self-rated health"

# Create data sets -------------------------------------------------------

l_srh <- 1:7 |>
  purrr::map(\(x) d_srh |> dplyr::filter(wave_num == x))

l_srh_low <- l_srh |>
  purrr::map(\(x) x |> dplyr::filter(class %in% c("low", "lower-middle")))

# Fit models -------------------------------------------------------------

l_srh_cbc <- l_srh |>
  purrr::map(\(x) {
    ordinal::clm(
      srh ~ cbc +
        age_group +
        gender +
        job +
        tu_member +
        tud +
        v2x_libdem +
        gini,
      data = x,
      link = "logit"
    )
  })

l_srh_cbc |> purrr::map(summary)
# [[1]] only 3 countries => max of 2 country-specific variables (vdem, gini are
#  NA)
# [[6]] only 2 countries => max of 1 country-specific variable (cbc, vdem, gini
#  are NA)

l_srh_cbc[[1]] <- NA
l_srh_cbc[[6]] <- NA

sensitivity_srh_cbc <- d_srh |>
  dplyr::select(wave, year) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::summarise(
    year = mean(year),
    .by = wave
  ) |>
  dplyr::mutate(
    wave = as.numeric(wave),
    model = "no interaction"
  ) |>
  dplyr::left_join(
    l_srh_cbc |>
      purrr::imap(\(x, idx) {
        if (is.logical(x)) {
          if (is.na(x)) {
            data.frame(
              term = "cbc",
              estimate = NA_real_,
              lwr = NA_real_,
              upr = NA_real_,
              wave = idx
            )
          } else {
            NA
          }
        } else {
          x |>
            broom::tidy() |>
            dplyr::filter(term == "cbc") |>
            dplyr::select(term, estimate, std.error) |>
            dplyr::mutate(
              lwr = exp(estimate - 1.96 * std.error),
              upr = exp(estimate + 1.96 * std.error),
              estimate = exp(estimate),
              std.error = NULL,
              wave = idx
            )
        }
      }) |>
      purrr::list_rbind(),
    by = "wave"
  )

l_srh_cbc_low <- l_srh_low |>
  purrr::map(\(x) {
    if (nrow(x) > 0) {
      ordinal::clm(
        srh ~ cbc +
          age_group +
          gender +
          job +
          tu_member +
          tud +
          v2x_libdem +
          gini,
        data = x,
        link = "logit"
      )
    } else {
      NA
    }
  })

l_srh_cbc_low |> purrr::map(summary)
# [[2]] only 3 countries => max of 2 country-specific variables (vdem, gini are NA)

l_srh_cbc_low[[2]] <- NA

sensitivity_srh_cbc_low <- d_srh |>
  dplyr::select(wave, year) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::summarise(
    year = mean(year),
    .by = wave
  ) |>
  dplyr::mutate(
    wave = as.numeric(wave),
    model = "no interaction"
  ) |>
  dplyr::left_join(
    l_srh_cbc_low |>
      purrr::imap(\(x, idx) {
        if (is.logical(x)) {
          if (is.na(x)) {
            data.frame(
              term = "cbc",
              estimate = NA_real_,
              lwr = NA_real_,
              upr = NA_real_,
              wave = idx
            )
          } else {
            NA
          }
        } else {
          x |>
            broom::tidy() |>
            dplyr::filter(term == "cbc") |>
            dplyr::select(term, estimate, std.error) |>
            dplyr::mutate(
              lwr = exp(estimate - 1.96 * std.error),
              upr = exp(estimate + 1.96 * std.error),
              estimate = exp(estimate),
              std.error = NULL,
              wave = idx
            )
        }
      }) |>
      purrr::list_rbind(),
    by = "wave"
  )

# Estimate from primary model --------------------------------------------

primary_srh_cbc <- d_srh |>
  dplyr::select(wave, year) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::mutate(
    wave = as.numeric(wave),
    term = paste0(
      "CBC (",
      year,
      ")"
    ),
    model = "with interaction"
  )

primary_srh_cbc <- primary_srh_cbc |>
  dplyr::left_join(
    SrhCbc3$fit |>
      functionsUnionHealth::coefs_clm(
        expo_mean = cbc_mean_srh,
        expo_sd = cbc_sd_srh,
        fixed_year = unique(primary_srh_cbc$year)
      ) |>
      dplyr::filter(stringi::stri_detect_regex(term, "^CBC")) |>
      dplyr::select(-coef.type),
    by = "term"
  ) |>
  dplyr::mutate(
    term = "cbc"
  )

srh_cbc <- vctrs::vec_rbind(sensitivity_srh_cbc, primary_srh_cbc)
rm(sensitivity_srh_cbc, primary_srh_cbc)

primary_srh_cbc_low <- d_srh |>
  dplyr::select(wave, year) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::mutate(
    wave = as.numeric(wave),
    term = paste0(
      "CBC (",
      year,
      ")"
    ),
    model = "with interaction"
  )

primary_srh_cbc_low <- primary_srh_cbc_low |>
  dplyr::left_join(
    SrhCbc3Low$fit |>
      functionsUnionHealth::coefs_clm(
        expo_mean = cbc_mean_srh,
        expo_sd = cbc_sd_srh,
        fixed_year = unique(primary_srh_cbc_low$year)
      ) |>
      dplyr::filter(stringi::stri_detect_regex(term, "^CBC")) |>
      dplyr::select(-coef.type),
    by = "term"
  ) |>
  dplyr::mutate(
    term = "cbc"
  )

srh_cbc_low <- vctrs::vec_rbind(sensitivity_srh_cbc_low, primary_srh_cbc_low)
rm(sensitivity_srh_cbc_low, primary_srh_cbc_low)

# Plot -------------------------------------------------------------------

p1 <- srh_cbc |>
  ggplot2::ggplot() +
  ggplot2::geom_pointrange(
    ggplot2::aes(
      x = estimate,
      xmin = lwr,
      xmax = upr,
      y = year
    ),
    size = 0.2
  ) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~model) +
  ggplot2::labs(
    title = "All countries",
    y = "Year",
    x = "OR (95%-CI)"
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 12),
    title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 12)) +
  ggplot2::theme_sub_plot(title = ggplot2::element_text(size = 15)) +
  ggplot2::coord_cartesian(xlim = c(0, 4))

p2 <- srh_cbc_low |>
  ggplot2::ggplot() +
  ggplot2::geom_pointrange(
    ggplot2::aes(
      x = estimate,
      xmin = lwr,
      xmax = upr,
      y = year
    ),
    size = 0.2
  ) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~model) +
  ggplot2::labs(
    title = "LMIC only",
    y = "Year",
    x = "OR (95%-CI)"
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 12),
    title = ggplot2::element_text(size = 12)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 12)) +
  ggplot2::theme_sub_plot(title = ggplot2::element_text(size = 15)) +
  ggplot2::coord_cartesian(xlim = c(0, 4))

p <- patchwork::wrap_plots(p1, p2, ncol = 1) +
  patchwork::plot_annotation(
    title = plot_title,
    theme = ggplot2::theme_sub_plot(title = ggplot2::element_text(size = 15))
  )
rm(srh_cbc, srh_cbc_low, p1, p2)

ggplot2::ggsave(
  filename = file.path("Output", "figure-s8c.jpg"),
  plot = p,
  units = "cm",
  width = 20,
  height = 20,
  dpi = 300
)
rm(p)
