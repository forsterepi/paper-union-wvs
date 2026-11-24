# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_3_3_sens-weighted

# Script Description: Sensitivity analysis: Weighted analysis. This script
#  creates Figure S7.
# - SrhTud3Wc: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in weighted data
# - SwlTud3Wc: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in weighted data
# - SrhCbc3Wc: Re-fit fully-adjusted model for self-rated health ~ collective
#   bargaining coverage in weighted data
# - SwlCbc3Wc: Re-fit fully-adjusted model for life satisfaction ~ collective
#   bargaining coverage in weighted data
# - Plot: Create Figure S7

# SrhTud3Wc --------------------------------------------------------------

if (cache$exists("SrhTud3Wc")) {
  SrhTud3Wc <- cache$get("SrhTud3Wc")
} else {
  cat("--- Fit model SrhTud3Wc (1/4 in post_3_3_sens-weighted.R) ---\n")

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
    weights = w_c,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhTud3Wc <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = tud_mean_srh,
    expo_sd = tud_sd_srh,
    seed = 160302,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3Wc", SrhTud3Wc)
}

# SwlTud3Wc --------------------------------------------------------------

if (cache$exists("SwlTud3Wc")) {
  SwlTud3Wc <- cache$get("SwlTud3Wc")
} else {
  cat("--- Fit model SwlTud3Wc (2/4 in post_3_3_sens-weighted.R) ---\n")

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
    weights = w_c,
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlTud3Wc <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = tud_mean_swl,
    expo_sd = tud_sd_swl,
    seed = 275774,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3Wc", SwlTud3Wc)
}

# SrhCbc3Wc --------------------------------------------------------------

if (cache$exists("SrhCbc3Wc")) {
  SrhCbc3Wc <- cache$get("SrhCbc3Wc")
} else {
  cat("--- Fit model SrhCbc3Wc (3/4 in post_3_3_sens-weighted.R) ---\n")

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
    weights = w_c,
    data = d_srh,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3Wc <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh,
    expo_mean = cbc_mean_srh,
    expo_sd = cbc_sd_srh,
    seed = 854044,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3Wc", SrhCbc3Wc)
}

# SwlCbc3Wc --------------------------------------------------------------

if (cache$exists("SwlCbc3Wc")) {
  SwlCbc3Wc <- cache$get("SwlCbc3Wc")
} else {
  cat("--- Fit model SwlCbc3Wc (4/4 in post_3_3_sens-weighted.R) ---\n")

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
    weights = w_c,
    data = d_swl,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3Wc <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl,
    expo_mean = cbc_mean_swl,
    expo_sd = cbc_sd_swl,
    seed = 989832,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3Wc", SwlCbc3Wc)
}

# Plot -------------------------------------------------------------------

p_est_weighted <- vctrs::vec_rbind(
  SrhTud3$coefs |>
    dplyr::mutate(
      model = "SrhTud3"
    ),
  SrhTud3Wc$coefs |>
    dplyr::mutate(
      model = "SrhTud3Wc"
    ),
  SwlTud3$coefs |>
    dplyr::mutate(
      model = "SwlTud3"
    ),
  SwlTud3Wc$coefs |>
    dplyr::mutate(
      model = "SwlTud3Wc"
    ),
  SrhCbc3$coefs |>
    dplyr::mutate(
      model = "SrhCbc3"
    ),
  SrhCbc3Wc$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Wc"
    ),
  SwlCbc3$coefs |>
    dplyr::mutate(
      model = "SwlCbc3"
    ),
  SwlCbc3Wc$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Wc"
    )
) |>
  dplyr::filter(coef.type == "inter_expo") |>
  dplyr::mutate(
    model_type = model |>
      stringi::stri_sub(from = 7) |>
      factor(
        levels = c("3", "3Wc"),
        labels = c("Unweighted", "Weighted")
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
  filename = file.path("Output", "figure-s7.jpg"),
  plot = p_est_weighted,
  units = "cm",
  width = 24,
  height = 12,
  dpi = 300
)
