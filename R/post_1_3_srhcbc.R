# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_1_3_srhcbc

# Script Description: Fit, evaluate, and compare all SrhCbc models (self-rated
#  health ~ collective bargaining coverage)
# - Define labels: Create labels for prediction sub-plot
# - SrhCbc0: Fit and evaluate model without adjustment
# - SrhCbc1: Fit and evaluate model with individual-level adjustment
# - SrhCbc2: Fit and evaluate model with additional exposure-related adjustment
# - SrhCbc3: Fit and evaluate fully-adjusted model
# - Subset by country income: Create data subset that only includes
#   low/lower-middle income countries
# - SrhCbc3Low: Fit and evaluate fully-adjusted model in data from
#   low/lower-middle income countries only
# - Prediction sub-plot: Create subplot of Figure 3 (combined in later script)
# - Extract values for manuscript: Extract values for description of Figure 3 in
#   Results section
# - Clean: Remove objects that are not needed anymore

# Define labels ----------------------------------------------------------

axis_exposure <- "Collective bargaining coverage"
axis_outcome <- "Expected self-rated health (higher is worse)"

# SrhCbc0 ----------------------------------------------------------------

if (cache$exists("SrhCbc0")) {
  SrhCbc0 <- cache$get("SrhCbc0")
} else {
  cat("--- Fit model SrhCbc0 (1/5 in post_1_3_srhcbc.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ cbc * decades_since_start,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhCbc0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 455183,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc0", SrhCbc0)
}

# SrhCbc1 ----------------------------------------------------------------

if (cache$exists("SrhCbc1")) {
  SrhCbc1 <- cache$get("SrhCbc1")
} else {
  cat("--- Fit model SrhCbc1 (2/5 in post_1_3_srhcbc.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ cbc * decades_since_start + age_group + gender + job,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhCbc1 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 824072,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc1", SrhCbc1)
}

# SrhCbc2 ----------------------------------------------------------------

if (cache$exists("SrhCbc2")) {
  SrhCbc2 <- cache$get("SrhCbc2")
} else {
  cat("--- Fit model SrhCbc2 (3/5 in post_1_3_srhcbc.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ cbc *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      tud,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhCbc2 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 889837,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc2", SrhCbc2)
}

# SrhCbc3 ----------------------------------------------------------------

if (cache$exists("SrhCbc3")) {
  SrhCbc3 <- cache$get("SrhCbc3")
} else {
  cat("--- Fit model SrhCbc3 (4/5 in post_1_3_srhcbc.R) ---\n")

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
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 107045
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3", SrhCbc3)
}

# Subset by country income -----------------------------------------------

d_srh_low <- d_srh |>
  dplyr::filter(class %in% c("low", "lower-middle"))

# SrhCbc3Low -------------------------------------------------------------

if (cache$exists("SrhCbc3Low")) {
  SrhCbc3Low <- cache$get("SrhCbc3Low")
} else {
  cat("--- Fit model SrhCbc3Low (5/5 in post_1_3_srhcbc.R) ---\n")

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
    data = d_srh_low,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3Low <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_low,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 452395
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3Low", SrhCbc3Low)
}

# Prediction sub-plot ----------------------------------------------------

## Models with all countries ----
pred_low <- SrhCbc3$pred |>
  dplyr::filter(class == "low") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-97)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-07)` = "5"
      )
  )

# (Adjust cbc to get a systematic and controlled jitter)
distance <- 0.025
pred_low <- pred_low |>
  dplyr::group_by(wave, cbc) |>
  dplyr::arrange(dplyr::desc(mean_sampled)) |>
  dplyr::mutate(
    rownr = 1:dplyr::n(),
    max_rownr = max(rownr)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    rownr = (rownr - 1) %% 2,
    cbc_adj = cbc + rownr * distance
  )
rm(distance)

## Observed values ----
obs <- SrhCbc3$fit$model |>
  dplyr::summarise(
    mean_srh = srh |> as.integer() |> mean(),
    cbc = dplyr::first(cbc),
    .by = c(decades_since_start, country)
  ) |>
  dplyr::mutate(
    year = decades_since_start |>
      magrittr::multiply_by(10) |>
      magrittr::add(1981) |>
      round(0)
  ) |>
  dplyr::left_join(
    d_srh |>
      dplyr::select(country, year, class, c, wave) |>
      plyr::count() |>
      dplyr::select(-freq),
    by = c("country", "year")
  ) |>
  dplyr::filter(class == "low") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-97)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-07)` = "5"
      )
  )

## Models with low/lower-middle only ----
pred_stratified_low <- SrhCbc3Low$pred |>
  dplyr::filter(class == "low") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-97)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-07)` = "5"
      )
  )

# (Adjust cbc to get a systematic and controlled jitter)
distance <- 0.02
pred_stratified_low <- pred_stratified_low |>
  dplyr::group_by(wave, cbc) |>
  dplyr::arrange(dplyr::desc(mean_sampled)) |>
  dplyr::mutate(
    rownr = 1:dplyr::n(),
    max_rownr = max(rownr)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    rownr = (rownr - 1) %% 2,
    cbc_adj = cbc + rownr * distance,
    cbc_adj3 = cbc + rownr * distance * 3
  )
rm(distance)

## Labels ----
pred_stratified_low_label <- pred_stratified_low |>
  dplyr::filter(cbc == max(cbc))

## Plot ----
p_srh_cbc_pred_low <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = cbc_adj, y = mean_sampled, color = c),
    linetype = "12",
    linewidth = 0.7
  ) +
  ggrepel::geom_label_repel(
    data = pred_stratified_low_label,
    mapping = ggplot2::aes(x = cbc, y = mean_sampled, label = c, fill = c),
    nudge_x = 0.4,
    min.segment.length = 0,
    xlim = c(max(pred_low$cbc) + 0.1, NA),
    alpha = 0.6
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = cbc_adj3, y = mean_sampled, color = c),
    linetype = "88",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = cbc_adj,
      ymin = q025_sampled,
      ymax = q975_sampled,
      color = c
    )
  ) +
  ggplot2::geom_point(
    data = obs,
    mapping = ggplot2::aes(x = cbc, y = mean_srh, color = c),
    size = 3
  ) +
  ggplot2::coord_cartesian(ylim = c(1, nlevels(d_srh$srh))) +
  ggplot2::scale_x_continuous(
    breaks = SrhCbc3$pred$cbc |>
      unique() |>
      sort(),
    labels = SrhCbc3$pred$cbc |>
      unique() |>
      sort() |>
      magrittr::multiply_by(cbc_sd_srh) |>
      magrittr::add(cbc_mean_srh) |>
      round(3) |>
      magrittr::multiply_by(100) |>
      paste0("%")
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "CHN" = "black",
      "MDA" = "red",
      "BGD" = "blue",
      "ARM" = "darkorange",
      "VNM" = "black",
      "UGA" = "darkorange",
      "RWA" = "red",
      "ETH" = "blue",
      "ZMB" = "darkorange",
      "GHA" = "deepskyblue1"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "CHN" = "black",
      "MDA" = "red",
      "BGD" = "blue",
      "ARM" = "darkorange",
      "VNM" = "black",
      "UGA" = "darkorange",
      "RWA" = "red",
      "ETH" = "blue",
      "ZMB" = "darkorange",
      "GHA" = "deepskyblue1"
    )
  ) +
  ggplot2::labs(
    x = axis_exposure,
    y = axis_outcome
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme_sub_legend(
    position = "none"
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 13)) +
  ggplot2::facet_wrap(~wave, ncol = 1)

# Extract values for manuscript ------------------------------------------

pred_values_srh_cbc_all <- pred_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, cbc, mean_sampled) |>
  dplyr::mutate(
    expo_val = cbc |>
      magrittr::multiply_by(cbc_sd_srh) |>
      magrittr::add(cbc_mean_srh) |>
      round(1),
    model = "all",
    expo = "cbc",
    outc = "srh",
    cbc = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_srh_cbc_low <- pred_stratified_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, cbc, mean_sampled) |>
  dplyr::mutate(
    expo_val = cbc |>
      magrittr::multiply_by(cbc_sd_srh) |>
      magrittr::add(cbc_mean_srh) |>
      round(1),
    model = "low",
    expo = "cbc",
    outc = "srh",
    cbc = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_srh_cbc <- vctrs::vec_rbind(
  pred_values_srh_cbc_all,
  pred_values_srh_cbc_low
) |>
  dplyr::arrange(c, model, expo_val)
rm(pred_values_srh_cbc_all, pred_values_srh_cbc_low)

# Clean ------------------------------------------------------------------

rm(
  d_srh_low,
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low_label,
  pred_stratified_low,
  obs
)
