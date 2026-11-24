# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_3_4_sens-by-employment-status

# Script Description: Sensitivity analysis: Stratify by employment status. This
#  script creates Figure S6.
# - Create data: Restrict data the employees only
# - SrhTud3Empl: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in restricted data
# - SrhTud3EmplLow: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in restricted data from low/lower-middle income countries only
# - SwlTud3Empl: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in restricted data
# - SwlTud3EmplLow: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in restricted data from low/lower-middle income countries only
# - SrhCbc3Empl: Re-fit fully-adjusted model for self-rated health ~ collective
#   bargaining coverage in restricted data
# - SrhCbc3EmplLow: Re-fit fully-adjusted model for self-rated health ~
#   collective bargaining coverage in restricted data from low/lower-middle
#   income countries only
# - SwlCbc3Empl: Re-fit fully-adjusted model for life satisfaction ~ collective
#   bargaining coverage in restricted data
# - SwlCbc3EmplLow: Re-fit fully-adjusted model for life satisfaction ~
#   collective bargaining coverage in restricted data from low/lower-middle
#   income countries only
# - Plot: Create Figure S6

# Create data ------------------------------------------------------------

## srh
d_srh_empl <- d_srh |>
  dplyr::filter(empl_stat %in% c("Full time", "Part time"))

d_srh_empl_low <- d_srh_empl |>
  dplyr::filter(class %in% c("low", "lower-middle"))

## swl
d_swl_empl <- d_swl |>
  dplyr::filter(empl_stat %in% c("Full time", "Part time"))

d_swl_empl_low <- d_swl_empl |>
  dplyr::filter(class %in% c("low", "lower-middle"))

# SrhTud3Empl -----------------------------------------------------------

if (cache$exists("SrhTud3Empl")) {
  SrhTud3Empl <- cache$get("SrhTud3Empl")
} else {
  cat(
    "--- Fit model SrhTud3Empl",
    "(1/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_srh_empl,
    link = "logit"
  )

  ## Evaluate
  SrhTud3Empl <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_empl,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 575100,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3Empl", SrhTud3Empl)
}

# SrhTud3EmplLow -----------------------------------------------------------

if (cache$exists("SrhTud3EmplLow")) {
  SrhTud3EmplLow <- cache$get("SrhTud3EmplLow")
} else {
  cat(
    "--- Fit model SrhTud3EmplLow",
    "(2/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_srh_empl_low,
    link = "logit"
  )

  ## Evaluate
  SrhTud3EmplLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_empl_low,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 402083,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3EmplLow", SrhTud3EmplLow)
}

# SwlTud3Empl -----------------------------------------------------------

if (cache$exists("SwlTud3Empl")) {
  SwlTud3Empl <- cache$get("SwlTud3Empl")
} else {
  cat(
    "--- Fit model SwlTud3Empl",
    "(3/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_swl_empl,
    link = "logit"
  )

  ## Evaluate
  SwlTud3Empl <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_empl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 45891,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3Empl", SwlTud3Empl)
}

# SwlTud3EmplLow -----------------------------------------------------------

if (cache$exists("SwlTud3EmplLow")) {
  SwlTud3EmplLow <- cache$get("SwlTud3EmplLow")
} else {
  cat(
    "--- Fit model SwlTud3EmplLow",
    "(4/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_swl_empl_low,
    link = "logit"
  )

  ## Evaluate
  SwlTud3EmplLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_empl_low,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 148291,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3EmplLow", SwlTud3EmplLow)
}

# SrhCbc3Empl -----------------------------------------------------------

if (cache$exists("SrhCbc3Empl")) {
  SrhCbc3Empl <- cache$get("SrhCbc3Empl")
} else {
  cat(
    "--- Fit model SrhCbc3Empl",
    "(5/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_srh_empl,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3Empl <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_empl,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 355847,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3Empl", SrhCbc3Empl)
}

# SrhCbc3EmplLow -----------------------------------------------------------

if (cache$exists("SrhCbc3EmplLow")) {
  SrhCbc3EmplLow <- cache$get("SrhCbc3EmplLow")
} else {
  cat(
    "--- Fit model SrhCbc3EmplLow",
    "(6/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_srh_empl_low,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3EmplLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_empl_low,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 831152,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3EmplLow", SrhCbc3EmplLow)
}

# SwlCbc3Empl -----------------------------------------------------------

if (cache$exists("SwlCbc3Empl")) {
  SwlCbc3Empl <- cache$get("SwlCbc3Empl")
} else {
  cat(
    "--- Fit model SwlCbc3Empl",
    "(7/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_swl_empl,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3Empl <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_empl,
    expo_mean = cbc_mean_swl,
    expo_sd = cbc_sd_swl,
    seed = 254433,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3Empl", SwlCbc3Empl)
}

# SwlCbc3EmplLow -----------------------------------------------------------

if (cache$exists("SwlCbc3EmplLow")) {
  SwlCbc3EmplLow <- cache$get("SwlCbc3EmplLow")
} else {
  cat(
    "--- Fit model SwlCbc3EmplLow",
    "(8/8 in post_3_4_sens-by-employment-status.R) ---\n"
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
    data = d_swl_empl_low,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3EmplLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_empl_low,
    expo_mean = cbc_mean_swl,
    expo_sd = cbc_sd_swl,
    seed = 96509,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3EmplLow", SwlCbc3EmplLow)
}

# Plot -------------------------------------------------------------------

p_est_empl <- vctrs::vec_rbind(
  SrhTud3$coefs |>
    dplyr::mutate(
      model = "SrhTud3"
    ),
  SrhTud3Empl$coefs |>
    dplyr::mutate(
      model = "SrhTud3Empl"
    ),
  SrhTud3Low$coefs |>
    dplyr::mutate(
      model = "SrhTud3Low"
    ),
  SrhTud3EmplLow$coefs |>
    dplyr::mutate(
      model = "SrhTud3EmplLow"
    ),
  SwlTud3$coefs |>
    dplyr::mutate(
      model = "SwlTud3"
    ),
  SwlTud3Empl$coefs |>
    dplyr::mutate(
      model = "SwlTud3Empl"
    ),
  SwlTud3Low$coefs |>
    dplyr::mutate(
      model = "SwlTud3Low"
    ),
  SwlTud3EmplLow$coefs |>
    dplyr::mutate(
      model = "SwlTud3EmplLow"
    ),
  SrhCbc3$coefs |>
    dplyr::mutate(
      model = "SrhCbc3"
    ),
  SrhCbc3Empl$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Empl"
    ),
  SrhCbc3Low$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Low"
    ),
  SrhCbc3EmplLow$coefs |>
    dplyr::mutate(
      model = "SrhCbc3EmplLow"
    ),
  SwlCbc3$coefs |>
    dplyr::mutate(
      model = "SwlCbc3"
    ),
  SwlCbc3Empl$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Empl"
    ),
  SwlCbc3Low$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Low"
    ),
  SwlCbc3EmplLow$coefs |>
    dplyr::mutate(
      model = "SwlCbc3EmplLow"
    )
) |>
  dplyr::filter(coef.type == "inter_expo") |>
  dplyr::mutate(
    model_type = model |>
      stringi::stri_sub(from = 7) |>
      factor(
        levels = c("3", "3Empl", "3Low", "3EmplLow"),
        labels = c(
          "all",
          "all (employees only)",
          "low/lower-middle",
          "low/lower-middle (employees only)"
        )
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
  ggplot2::scale_color_manual(values = c("black", "red", "blue", "#94006a")) +
  ggplot2::theme_bw() +
  ggplot2::theme_sub_axis_y(title = ggplot2::element_blank()) +
  ggplot2::theme_sub_legend(
    position = "bottom",
    title = ggplot2::element_blank(),
    text = ggplot2::element_text(size = 13)
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
    x = "Odds Ratio (95% confidence interval)"
  ) +
  ggplot2::facet_wrap(~outcome)

ggplot2::ggsave(
  filename = file.path("Output", "figure-s6.jpg"),
  plot = p_est_empl,
  units = "cm",
  width = 24,
  height = 17,
  dpi = 300
)
