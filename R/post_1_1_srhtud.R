# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_1_1_srhtud

# Script Description: Fit, evaluate, and compare all SrhTud models (self-rated
#  health ~ trade union density)
# - Define labels: Create labels for prediction sub-plot
# - SrhTud0: Fit and evaluate model without adjustment
# - SrhTud1: Fit and evaluate model with individual-level adjustment
# - SrhTud2: Fit and evaluate model with additional exposure-related adjustment
# - SrhTud3: Fit and evaluate fully-adjusted model
# - Subset by country income: Create data subset that only includes
#   low/lower-middle income countries
# - SrhTud3Low: Fit and evaluate fully-adjusted model in data from
#   low/lower-middle income countries only
# - Prediction sub-plot: Create subplot of Figure 3 (combined in later script)
# - Extract values for manuscript: Extract values for description of Figure 3 in
#   Results section
# - Clean: Remove objects that are not needed anymore

# Define labels ----------------------------------------------------------

axis_exposure <- "Trade union density"
axis_outcome <- "Expected self-rated health (higher is worse)"

# SrhTud0 ----------------------------------------------------------------

if (cache$exists("SrhTud0")) {
  SrhTud0 <- cache$get("SrhTud0")
} else {
  cat("--- Fit model SrhTud0 (1/5 in post_1_1_srhtud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ tud * decades_since_start,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhTud0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 31931,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud0", SrhTud0)
}

# SrhTud1 ----------------------------------------------------------------

if (cache$exists("SrhTud1")) {
  SrhTud1 <- cache$get("SrhTud1")
} else {
  cat("--- Fit model SrhTud1 (2/5 in post_1_1_srhtud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ tud * decades_since_start + age_group + gender + job,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhTud1 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 432459,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud1", SrhTud1)
}

# SrhTud2 ----------------------------------------------------------------

if (cache$exists("SrhTud2")) {
  SrhTud2 <- cache$get("SrhTud2")
} else {
  cat("--- Fit model SrhTud2 (3/5 in post_1_1_srhtud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    srh ~ tud *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      cbc,
    nominal = ~country,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhTud2 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 490352,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud2", SrhTud2)
}

# SrhTud3 ----------------------------------------------------------------

if (cache$exists("SrhTud3")) {
  SrhTud3 <- cache$get("SrhTud3")
} else {
  cat("--- Fit model SrhTud3 (4/5 in post_1_1_srhtud.R) ---\n")

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
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhTud3 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 682793
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3", SrhTud3)
}

# Subset by country income -----------------------------------------------

d_srh_low <- d_srh |>
  dplyr::filter(class %in% c("low", "lower-middle"))

# SrhTud3Low -------------------------------------------------------------

if (cache$exists("SrhTud3Low")) {
  SrhTud3Low <- cache$get("SrhTud3Low")
} else {
  cat("--- Fit model SrhTud3Low (5/5 in post_1_1_srhtud.R) ---\n")

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
    data = d_srh_low,
    link = "logit"
  )

  ## Evaluate
  SrhTud3Low <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_low,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 104235
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3Low", SrhTud3Low)
}

# Prediction sub-plot ----------------------------------------------------

## Models with all countries ----
pred_low <- SrhTud3$pred |>
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

# (Adjust tud to get a systematic and controlled jitter)
distance <- 0.025
pred_low <- pred_low |>
  dplyr::group_by(wave, tud) |>
  dplyr::arrange(dplyr::desc(mean_sampled)) |>
  dplyr::mutate(
    rownr = 1:dplyr::n(),
    max_rownr = max(rownr)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    rownr = (rownr - 1) %% 2,
    tud_adj = tud + rownr * distance
  )
rm(distance)

## Observed values ----
obs <- SrhTud3$fit$model |>
  dplyr::summarise(
    mean_srh = srh |> as.integer() |> mean(),
    tud = dplyr::first(tud),
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
pred_stratified_low <- SrhTud3Low$pred |>
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

# (Adjust tud to get a systematic and controlled jitter)
distance <- 0.02
pred_stratified_low <- pred_stratified_low |>
  dplyr::group_by(wave, tud) |>
  dplyr::arrange(dplyr::desc(mean_sampled)) |>
  dplyr::mutate(
    rownr = 1:dplyr::n(),
    max_rownr = max(rownr)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    rownr = (rownr - 1) %% 2,
    tud_adj = tud + rownr * distance,
    tud_adj3 = tud + rownr * distance * 3
  )
rm(distance)

## Labels ----
pred_stratified_low_label <- pred_stratified_low |>
  dplyr::filter(tud == max(tud))

## Plot ----
p_srh_tud_pred_low <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = tud_adj, y = mean_sampled, color = c),
    linetype = "12",
    linewidth = 0.7
  ) +
  ggrepel::geom_label_repel(
    data = pred_stratified_low_label,
    mapping = ggplot2::aes(x = tud, y = mean_sampled, label = c, fill = c),
    nudge_x = 0.6,
    min.segment.length = 0,
    xlim = c(max(pred_low$tud) + 0.1, NA),
    alpha = 0.6
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = tud_adj3, y = mean_sampled, color = c),
    linetype = "88",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = tud_adj,
      ymin = q025_sampled,
      ymax = q975_sampled,
      color = c
    )
  ) +
  ggplot2::geom_point(
    data = obs,
    mapping = ggplot2::aes(x = tud, y = mean_srh, color = c),
    size = 3
  ) +
  ggplot2::coord_cartesian(ylim = c(1, nlevels(d_srh$srh))) +
  ggplot2::scale_x_continuous(
    breaks = SrhTud3$pred$tud |>
      unique() |>
      sort(),
    labels = SrhTud3$pred$tud |>
      unique() |>
      sort() |>
      magrittr::multiply_by(tud_sd_srh) |>
      magrittr::add(tud_mean_srh) |>
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

pred_values_srh_tud_all <- pred_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, tud, mean_sampled) |>
  dplyr::mutate(
    expo_val = tud |>
      magrittr::multiply_by(tud_sd_srh) |>
      magrittr::add(tud_mean_srh) |>
      round(1),
    model = "all",
    expo = "tud",
    outc = "srh",
    tud = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_srh_tud_low <- pred_stratified_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, tud, mean_sampled) |>
  dplyr::mutate(
    expo_val = tud |>
      magrittr::multiply_by(tud_sd_srh) |>
      magrittr::add(tud_mean_srh) |>
      round(1),
    model = "low",
    expo = "tud",
    outc = "srh",
    tud = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_srh_tud <- vctrs::vec_rbind(
  pred_values_srh_tud_all,
  pred_values_srh_tud_low
) |>
  dplyr::arrange(c, model, expo_val)
rm(pred_values_srh_tud_all, pred_values_srh_tud_low)

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
