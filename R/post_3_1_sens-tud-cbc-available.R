# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_3_1_sens-tud-cbc-available

# Script Description: Sensitivity analysis: Including only surveys for which
#  TUD and CBC are available for the same year. This script creates Figure S5.
# - Create data: Restrict data to rows with trade union density and collective
#   bargaining coverage values available for the year of data collection
# - SrhTud3Diff0: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in restricted data
# - SwlTud3Diff0: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in restricted data
# - SrhCbc3Diff0: Re-fit fully-adjusted model for self-rated health ~ collective
#   bargaining coverage in restricted data
# - SwlCbc3Diff0: Re-fit fully-adjusted model for life satisfaction ~ collective
#   bargaining coverage in restricted data
# - Plot: Create Figure S5

# Create data ------------------------------------------------------------

## srh
d_srh_diff_0 <- d_srh |>
  dplyr::filter(tud_year_diff == 0, cbc_year_diff == 0)

## swl
d_swl_diff_0 <- d_swl |>
  dplyr::filter(tud_year_diff == 0, cbc_year_diff == 0)

# There's not enough variability in country-level covariates for refitting
# the models in low/lower-middle income countries only.

# SrhTud3Diff0 -----------------------------------------------------------

if (cache$exists("SrhTud3Diff0")) {
  SrhTud3Diff0 <- cache$get("SrhTud3Diff0")
} else {
  cat(
    "--- Fit model SrhTud3Diff0",
    "(1/4 in post_3_1_sens-tud-cbc-available.R) ---\n"
  )

  ## Fit
  fit <- ordinal::clm(
    srh ~ tud *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      cbc +
      v2x_libdem +
      gini,
    nominal = ~country,
    data = d_srh_diff_0,
    link = "logit"
  )

  ## Evaluate
  SrhTud3Diff0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_diff_0,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 59755,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3Diff0", SrhTud3Diff0)
}

# SwlTud3Diff0 ----------------------------------------------------------------

if (cache$exists("SwlTud3Diff0")) {
  SwlTud3Diff0 <- cache$get("SwlTud3Diff0")
} else {
  cat(
    "--- Fit model SwlTud3Diff0",
    "(2/4 in post_3_1_sens-tud-cbc-available.R) ---\n"
  )

  ## Fit
  fit <- ordinal::clm(
    swl ~ tud *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      cbc +
      v2x_libdem +
      gini,
    nominal = ~country,
    data = d_swl_diff_0,
    link = "logit"
  )

  ## Evaluate
  SwlTud3Diff0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_diff_0,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 292132,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3Diff0", SwlTud3Diff0)
}

# SrhCbc3Diff0 ----------------------------------------------------------------

if (cache$exists("SrhCbc3Diff0")) {
  SrhCbc3Diff0 <- cache$get("SrhCbc3Diff0")
} else {
  cat(
    "--- Fit model SrhCbc3Diff0",
    "(3/4 in post_3_1_sens-tud-cbc-available.R) ---\n"
  )

  ## Fit
  fit <- ordinal::clm(
    srh ~ cbc *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      tud +
      v2x_libdem +
      gini,
    nominal = ~country,
    data = d_srh_diff_0,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3Diff0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_diff_0,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 809828,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3Diff0", SrhCbc3Diff0)
}

# SwlCbc3Diff0 ----------------------------------------------------------------

if (cache$exists("SwlCbc3Diff0")) {
  SwlCbc3Diff0 <- cache$get("SwlCbc3Diff0")
} else {
  cat(
    "--- Fit model SwlCbc3Diff0",
    "(4/4 in post_3_1_sens-tud-cbc-available.R) ---\n"
  )

  ## Fit
  fit <- ordinal::clm(
    swl ~ cbc *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      tud +
      v2x_libdem +
      gini,
    nominal = ~country,
    data = d_swl_diff_0,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3Diff0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_diff_0,
    expo_mean = cbc_mean_swl,
    expo_sd = cbc_sd_swl,
    seed = 945976,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3Diff0", SwlCbc3Diff0)
}

# Plot -------------------------------------------------------------------

## Estimates plot ----
p_est_diff0 <- vctrs::vec_rbind(
  SrhTud3$coefs |>
    dplyr::mutate(
      model = "SrhTud3"
    ),
  SrhTud3Diff0$coefs |>
    dplyr::mutate(
      model = "SrhTud3Diff0"
    ),
  SwlTud3$coefs |>
    dplyr::mutate(
      model = "SwlTud3"
    ),
  SwlTud3Diff0$coefs |>
    dplyr::mutate(
      model = "SwlTud3Diff0"
    ),
  SrhCbc3$coefs |>
    dplyr::mutate(
      model = "SrhCbc3"
    ),
  SrhCbc3Diff0$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Diff0"
    ),
  SwlCbc3$coefs |>
    dplyr::mutate(
      model = "SwlCbc3"
    ),
  SwlCbc3Diff0$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Diff0"
    )
) |>
  dplyr::filter(coef.type == "inter_expo") |>
  dplyr::mutate(
    model_type = model |>
      stringi::stri_sub(from = 7) |>
      factor(
        levels = c("3", "3Diff0"),
        labels = c("All surveys", "TUD/CBC available for same year")
      ),
    outcome = model |>
      stringi::stri_sub(to = 3) |>
      factor(
        levels = c("Srh", "Swl"),
        labels = c("Self-rated health", "Life satisfaction")
      ),
    exposure = model |>
      stringi::stri_sub(from = 4, to = 6) |>
      factor(
        levels = c("Tud", "Cbc")
      ),
    term = term |>
      as.factor() |>
      forcats::fct_relevel(
        "CBC (2015)",
        "CBC (2005)",
        "CBC (1995)",
        "CBC (1985)",
        "TUD (2015)",
        "TUD (2005)",
        "TUD (1995)",
        "TUD (1985)"
      )
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_hline(
    yintercept = seq(1.5, (7 + 0.5), by = 1),
    linewidth = 0.2,
    color = "grey"
  ) +
  ggplot2::geom_hline(yintercept = 4.5) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
  ggplot2::geom_pointrange(
    ggplot2::aes(
      x = estimate,
      xmin = lwr,
      xmax = upr,
      y = term,
      color = model_type
    ),
    position = ggplot2::position_dodge(width = 0.9)
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 2.5)) +
  ggplot2::scale_color_manual(values = c("black", "darkorange")) +
  ggplot2::theme_bw() +
  ggplot2::theme_sub_axis_y(title = ggplot2::element_blank()) +
  ggplot2::theme_sub_legend(
    position = "none"
  ) +
  ggplot2::theme_sub_panel(grid.major.y = ggplot2::element_blank()) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(size = 13),
    background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::labs(
    x = "Odds Ratio (95% confidence interval)",
    title = "A"
  ) +
  ggplot2::facet_wrap(~outcome)

## Class distribution plot ----
add_class <- vctrs::vec_rbind(
  d_srh |>
    dplyr::select(c, country, year),
  d_swl |>
    dplyr::select(c, country, year)
) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::left_join(
    country_classification |>
      dplyr::select(-class_bin, -country),
    by = c("c", "year")
  ) |>
  dplyr::mutate(
    decades_since_start = (year - min(year)) / 10
  )

# Note: Tud and Cbc variants of the same models have same nobs
p_est_diff0_count <- vctrs::vec_rbind(
  SrhTud3$fit$model |>
    dplyr::left_join(add_class, by = c("country", "decades_since_start")) |>
    dplyr::select(class) |>
    plyr::count() |>
    dplyr::rename(all_freq = freq) |>
    dplyr::mutate(
      all_perc = all_freq / SrhTud3$nobs
    ) |>
    dplyr::left_join(
      SrhTud3Diff0$fit$model |>
        dplyr::left_join(add_class, by = c("country", "decades_since_start")) |>
        dplyr::select(class) |>
        plyr::count() |>
        dplyr::rename(available_freq = freq) |>
        dplyr::mutate(
          available_perc = available_freq / SrhTud3Diff0$nobs
        ),
      by = "class"
    ) |>
    tidyr::replace_na(list(available_freq = 0, available_perc = 0)) |>
    tidyr::pivot_longer(
      cols = !class,
      names_to = c("model", ".value"),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      outcome = "srh"
    ),
  SwlTud3$fit$model |>
    dplyr::left_join(add_class, by = c("country", "decades_since_start")) |>
    dplyr::select(class) |>
    plyr::count() |>
    dplyr::rename(all_freq = freq) |>
    dplyr::mutate(
      all_perc = all_freq / SwlTud3$nobs
    ) |>
    dplyr::left_join(
      SwlTud3Diff0$fit$model |>
        dplyr::left_join(add_class, by = c("country", "decades_since_start")) |>
        dplyr::select(class) |>
        plyr::count() |>
        dplyr::rename(available_freq = freq) |>
        dplyr::mutate(
          available_perc = available_freq / SwlTud3Diff0$nobs
        ),
      by = "class"
    ) |>
    tidyr::replace_na(list(available_freq = 0, available_perc = 0)) |>
    tidyr::pivot_longer(
      cols = !class,
      names_to = c("model", ".value"),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      outcome = "swl"
    )
) |>
  dplyr::mutate(
    outcome = factor(
      outcome,
      levels = c("srh", "swl"),
      labels = c("Self-rated health", "Life satisfaction")
    ),
    model = factor(
      model,
      levels = c("all", "available"),
      labels = c("All surveys", "TUD/CBC available for same year")
    ),
    perc = perc |>
      magrittr::multiply_by(100) |>
      round(1) |>
      format(nsmall = 1) |>
      paste0("%")
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(
    ggplot2::aes(x = class, y = freq, fill = model),
    position = ggplot2::position_dodge2(width = 0.9)
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = class, y = freq + 1000, label = perc),
    position = ggplot2::position_dodge2(width = 0.9),
    vjust = 0,
    size = 3.2
  ) +
  ggplot2::scale_fill_manual(values = c("black", "darkorange")) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 1e4, 2e4, 3e4),
    labels = c("0", "10", "20", "30"),
    limits = c(0, 3.7e4)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme_sub_legend(
    position = "bottom",
    title = ggplot2::element_blank(),
    text = ggplot2::element_text(size = 13)
  ) +
  ggplot2::theme_sub_panel(grid.major.x = ggplot2::element_blank()) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_axis_x(
    text = ggplot2::element_text(size = 11)
  ) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(size = 13),
    background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::labs(
    x = "Country income",
    y = "Number of participants (in 1000)",
    title = "B"
  ) +
  ggplot2::facet_wrap(~outcome, nrow = 1)

## Combine and save ----
p_est_diff0_combined <- patchwork::wrap_plots(
  p_est_diff0,
  p_est_diff0_count,
  ncol = 1,
  heights = c(2, 1)
)

ggplot2::ggsave(
  filename = file.path("Output", "figure-s5.jpg"),
  plot = p_est_diff0_combined,
  units = "cm",
  width = 24,
  height = 18,
  dpi = 300
)
