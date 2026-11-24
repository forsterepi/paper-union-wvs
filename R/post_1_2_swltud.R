# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_1_2_swltud

# Script Description: Fit, evaluate, and compare all SwlTud models (life
#  satisfaction ~ trade union density)
# - Define labels: Create labels for prediction sub-plot
# - SwlTud0: Fit and evaluate model without adjustment
# - SwlTud1: Fit and evaluate model with individual-level adjustment
# - SwlTud2: Fit and evaluate model with additional exposure-related adjustment
# - SwlTud3: Fit and evaluate fully-adjusted model
# - Subset by country income: Create data subset that only includes
#   low/lower-middle income countries
# - SwlTud3Low: Fit and evaluate fully-adjusted model in data from
#   low/lower-middle income countries only
# - Prediction sub-plot: Create subplot of Figure 3 (combined in later script)
# - Extract values for manuscript: Extract values for description of Figure 3 in
#   Results section
# - Clean: Remove objects that are not needed anymore

# Define labels ----------------------------------------------------------

axis_exposure <- "Trade union density"
axis_outcome <- "Expected life satisfaction (higher is worse)"

# SwlTud0 ----------------------------------------------------------------

if (cache$exists("SwlTud0")) {
  SwlTud0 <- cache$get("SwlTud0")
} else {
  cat("--- Fit model SwlTud0 (1/5 in post_1_2_swltud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlTud0 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 127440,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud0", SwlTud0)
}

# SwlTud1 ----------------------------------------------------------------

if (cache$exists("SwlTud1")) {
  SwlTud1 <- cache$get("SwlTud1")
} else {
  cat("--- Fit model SwlTud1 (2/5 in post_1_2_swltud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    swl ~ tud * decades_since_start + age_group + gender + job,
    nominal = ~country,
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlTud1 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 841249,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud1", SwlTud1)
}

# SwlTud2 ----------------------------------------------------------------

if (cache$exists("SwlTud2")) {
  SwlTud2 <- cache$get("SwlTud2")
} else {
  cat("--- Fit model SwlTud2 (3/5 in post_1_2_swltud.R) ---\n")

  ## Fit
  fit <- ordinal::clm(
    swl ~ tud *
      decades_since_start +
      age_group +
      gender +
      job +
      tu_member +
      cbc,
    nominal = ~country,
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlTud2 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 211285,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud2", SwlTud2)
}

# SwlTud3 ----------------------------------------------------------------

if (cache$exists("SwlTud3")) {
  SwlTud3 <- cache$get("SwlTud3")
} else {
  cat("--- Fit model SwlTud3 (4/5 in post_1_2_swltud.R) ---\n")

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
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlTud3 <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 419933
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3", SwlTud3)
}

# Subset by country income -----------------------------------------------

d_swl_low <- d_swl |>
  dplyr::filter(class %in% c("low", "lower-middle"))

# SwlTud3Low -------------------------------------------------------------

if (cache$exists("SwlTud3Low")) {
  SwlTud3Low <- cache$get("SwlTud3Low")
} else {
  cat("--- Fit model SwlTud3Low (5/5 in post_1_2_swltud.R) ---\n")

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
    data = d_swl_low,
    link = "logit"
  )

  ## Evaluate
  SwlTud3Low <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_low,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 86483
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3Low", SwlTud3Low)
}

# Prediction sub-plot ----------------------------------------------------

## Models with all countries ----
pred_low <- SwlTud3$pred |>
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
obs <- SwlTud3$fit$model |>
  dplyr::summarise(
    mean_swl = swl |> as.integer() |> mean(),
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
    d_swl |>
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
pred_stratified_low <- SwlTud3Low$pred |>
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
p_swl_tud_pred_low <- ggplot2::ggplot() +
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
    mapping = ggplot2::aes(x = tud, y = mean_swl, color = c),
    size = 3
  ) +
  ggplot2::coord_cartesian(ylim = c(1, nlevels(d_swl$swl))) +
  ggplot2::scale_x_continuous(
    breaks = SwlTud3$pred$tud |>
      unique() |>
      sort(),
    labels = SwlTud3$pred$tud |>
      unique() |>
      sort() |>
      magrittr::multiply_by(tud_sd_swl) |>
      magrittr::add(tud_mean_swl) |>
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

pred_values_swl_tud_all <- pred_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, tud, mean_sampled) |>
  dplyr::mutate(
    expo_val = tud |>
      magrittr::multiply_by(tud_sd_swl) |>
      magrittr::add(tud_mean_swl) |>
      round(1),
    model = "all",
    expo = "tud",
    outc = "swl",
    tud = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_swl_tud_low <- pred_stratified_low |>
  dplyr::filter(
    (wave == "Wave 2 (1990)" & c == "CHN") |
      (wave == "Wave 5 (2006-07)" & c == "RWA")
  ) |>
  dplyr::select(country, c, year, wave, tud, mean_sampled) |>
  dplyr::mutate(
    expo_val = tud |>
      magrittr::multiply_by(tud_sd_swl) |>
      magrittr::add(tud_mean_swl) |>
      round(1),
    model = "low",
    expo = "tud",
    outc = "swl",
    tud = NULL
  ) |>
  dplyr::filter(expo_val == 0.2 | expo_val == 0.8)

pred_values_swl_tud <- vctrs::vec_rbind(
  pred_values_swl_tud_all,
  pred_values_swl_tud_low
) |>
  dplyr::arrange(c, model, expo_val)
rm(pred_values_swl_tud_all, pred_values_swl_tud_low)

# Clean ------------------------------------------------------------------

rm(
  d_swl_low,
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low_label,
  pred_stratified_low,
  obs
)
