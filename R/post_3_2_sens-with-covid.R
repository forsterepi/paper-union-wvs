# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_3_2_sens-with-covid

# Script Description: Sensitivity analysis: Include post-2019 data that has been
#  excluded in the main analysis. This script creates Figure S4.
# - Prepare data: Include post-2019 data that has been excluded for the main
#   analyses
# - SrhTud3Covid: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in post-2019 data
# - SrhTud3CovidLow: Re-fit fully-adjusted model for self-rated health ~ trade
#   union density in post-2019 data from low/lower-middle income countries only
# - SwlTud3Covid: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in post-2019 data
# - SwlTud3CovidLow: Re-fit fully-adjusted model for life satisfaction ~ trade
#   union density in post-2019 data from low/lower-middle income countries only
# - SrhCbc3Covid: Re-fit fully-adjusted model for self-rated health ~ collective
#   bargaining coverage in post-2019 data
# - SrhCbc3CovidLow: Re-fit fully-adjusted model for self-rated health ~
#   collective bargaining coverage in post-2019 data from low/lower-middle
#   income countries only
# - SwlCbc3Covid: Re-fit fully-adjusted model for life satisfaction ~ collective
#   bargaining coverage in post-2019 data
# - SwlCbc3CovidLow: Re-fit fully-adjusted model for life satisfaction ~
#   collective bargaining coverage in post-2019 data from low/lower-middle
#   income countries only
# - Plot: Create Figure S4

# Prepare data -----------------------------------------------------------

## Exclude DomRep in srh ----
d_srh_covid <- d_srh_covid |>
  # Exclude the Dominican Republic because noone there has poor or very
  # poor self-rated health
  dplyr::filter(country != "Dominican Republic")

## Standardise continuous variables ----

## Get mean and SD for reporting
tud_mean_srh_covid <- mean(d_srh_covid$tud)
cbc_mean_srh_covid <- mean(d_srh_covid$cbc)
tud_sd_srh_covid <- sd(d_srh_covid$tud)
cbc_sd_srh_covid <- sd(d_srh_covid$cbc)

tud_mean_swl_covid <- mean(d_swl_covid$tud)
cbc_mean_swl_covid <- mean(d_swl_covid$cbc)
tud_sd_swl_covid <- sd(d_swl_covid$tud)
cbc_sd_swl_covid <- sd(d_swl_covid$cbc)

# Do standardisation
d_srh_covid <- d_srh_covid |>
  dplyr::mutate(
    tud_original = tud,
    cbc_original = cbc,
    v2x_libdem_original = v2x_libdem,
    gini_original = gini,
    tud = (tud - mean(tud)) / sd(tud),
    cbc = (cbc - mean(cbc)) / sd(cbc),
    v2x_libdem = (v2x_libdem - mean(v2x_libdem)) / sd(v2x_libdem),
    gini = (gini - mean(gini, na.rm = TRUE)) / sd(gini, na.rm = TRUE)
  )

d_swl_covid <- d_swl_covid |>
  dplyr::mutate(
    tud_original = tud,
    cbc_original = cbc,
    v2x_libdem_original = v2x_libdem,
    gini_original = gini,
    tud = (tud - mean(tud)) / sd(tud),
    cbc = (cbc - mean(cbc)) / sd(cbc),
    v2x_libdem = (v2x_libdem - mean(v2x_libdem)) / sd(v2x_libdem),
    gini = (gini - mean(gini, na.rm = TRUE)) / sd(gini, na.rm = TRUE)
  )

## Subset ----
d_srh_covid_low <- d_srh_covid |>
  dplyr::filter(class %in% c("low", "lower-middle"))

d_swl_covid_low <- d_swl_covid |>
  dplyr::filter(class %in% c("low", "lower-middle"))

# SrhTud3Covid ----------------------------------------------------------------

if (cache$exists("SrhTud3Covid")) {
  SrhTud3Covid <- cache$get("SrhTud3Covid")
} else {
  cat(
    "--- Fit model SrhTud3Covid",
    "(1/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_srh_covid,
    link = "logit"
  )

  ## Evaluate
  SrhTud3Covid <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_covid,
    expo_mean = tud_mean_srh_covid,
    expo_sd = tud_sd_srh_covid,
    seed = 558154,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3Covid", SrhTud3Covid)
}

# SrhTud3CovidLow -------------------------------------------------------------

if (cache$exists("SrhTud3CovidLow")) {
  SrhTud3CovidLow <- cache$get("SrhTud3CovidLow")
} else {
  cat(
    "--- Fit model SrhTud3CovidLow",
    "(2/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_srh_covid_low,
    link = "logit"
  )

  ## Evaluate
  SrhTud3CovidLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_covid_low,
    expo_mean = tud_mean_srh_covid,
    expo_sd = tud_sd_srh_covid,
    seed = 150749,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhTud3CovidLow", SrhTud3CovidLow)
}

# SwlTud3Covid ----------------------------------------------------------------

if (cache$exists("SwlTud3Covid")) {
  SwlTud3Covid <- cache$get("SwlTud3Covid")
} else {
  cat(
    "--- Fit model SwlTud3Covid",
    "(3/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_swl_covid,
    link = "logit"
  )

  ## Evaluate
  SwlTud3Covid <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_covid,
    expo_mean = tud_mean_swl_covid,
    expo_sd = tud_sd_swl_covid,
    seed = 352185,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3Covid", SwlTud3Covid)
}

# SwlTud3CovidLow -------------------------------------------------------------

if (cache$exists("SwlTud3CovidLow")) {
  SwlTud3CovidLow <- cache$get("SwlTud3CovidLow")
} else {
  cat(
    "--- Fit model SwlTud3CovidLow",
    "(4/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_swl_covid_low,
    link = "logit"
  )

  ## Evaluate
  SwlTud3CovidLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_covid_low,
    expo_mean = tud_mean_swl_covid,
    expo_sd = tud_sd_swl_covid,
    seed = 707586,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlTud3CovidLow", SwlTud3CovidLow)
}

# SrhCbc3Covid ----------------------------------------------------------------

if (cache$exists("SrhCbc3Covid")) {
  SrhCbc3Covid <- cache$get("SrhCbc3Covid")
} else {
  cat(
    "--- Fit model SrhCbc3Covid",
    "(5/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_srh_covid,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3Covid <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_covid,
    expo_mean = cbc_mean_srh_covid,
    expo_sd = cbc_sd_srh_covid,
    seed = 868215,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3Covid", SrhCbc3Covid)
}

# SrhCbc3CovidLow -------------------------------------------------------------

if (cache$exists("SrhCbc3CovidLow")) {
  SrhCbc3CovidLow <- cache$get("SrhCbc3CovidLow")
} else {
  cat(
    "--- Fit model SrhCbc3CovidLow",
    "(6/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_srh_covid_low,
    link = "logit"
  )

  ## Evaluate
  SrhCbc3CovidLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_srh_covid_low,
    expo_mean = cbc_mean_srh_covid,
    expo_sd = cbc_sd_srh_covid,
    seed = 274829,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SrhCbc3CovidLow", SrhCbc3CovidLow)
}

# SwlCbc3Covid ----------------------------------------------------------------

if (cache$exists("SwlCbc3Covid")) {
  SwlCbc3Covid <- cache$get("SwlCbc3Covid")
} else {
  cat(
    "--- Fit model SwlCbc3Covid",
    "(7/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_swl_covid,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3Covid <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_covid,
    expo_mean = cbc_mean_swl_covid,
    expo_sd = cbc_sd_swl_covid,
    seed = 11182,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3Covid", SwlCbc3Covid)
}

# SwlCbc3CovidLow -------------------------------------------------------------

if (cache$exists("SwlCbc3CovidLow")) {
  SwlCbc3CovidLow <- cache$get("SwlCbc3CovidLow")
} else {
  cat(
    "--- Fit model SwlCbc3CovidLow",
    "(8/8 in post_3_2_sens-with-covid.R) ---\n"
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
    data = d_swl_covid_low,
    link = "logit"
  )

  ## Evaluate
  SwlCbc3CovidLow <- functionsUnionHealth::evaluate_clm(
    fit = fit,
    data = d_swl_covid_low,
    expo_mean = cbc_mean_swl_covid,
    expo_sd = cbc_sd_swl_covid,
    seed = 670956,
    predict = FALSE
  )
  rm(fit)

  ## Cache
  cache$set("SwlCbc3CovidLow", SwlCbc3CovidLow)
}

# Plot -------------------------------------------------------------------

p_est_covid <- vctrs::vec_rbind(
  SrhTud3$coefs |>
    dplyr::mutate(
      model = "SrhTud3"
    ),
  SrhTud3Covid$coefs |>
    dplyr::mutate(
      model = "SrhTud3Covid"
    ),
  SrhTud3Low$coefs |>
    dplyr::mutate(
      model = "SrhTud3Low"
    ),
  SrhTud3CovidLow$coefs |>
    dplyr::mutate(
      model = "SrhTud3CovidLow"
    ),
  SwlTud3$coefs |>
    dplyr::mutate(
      model = "SwlTud3"
    ),
  SwlTud3Covid$coefs |>
    dplyr::mutate(
      model = "SwlTud3Covid"
    ),
  SwlTud3Low$coefs |>
    dplyr::mutate(
      model = "SwlTud3Low"
    ),
  SwlTud3CovidLow$coefs |>
    dplyr::mutate(
      model = "SwlTud3CovidLow"
    ),
  SrhCbc3$coefs |>
    dplyr::mutate(
      model = "SrhCbc3"
    ),
  SrhCbc3Covid$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Covid"
    ),
  SrhCbc3Low$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Low"
    ),
  SrhCbc3CovidLow$coefs |>
    dplyr::mutate(
      model = "SrhCbc3CovidLow"
    ),
  SwlCbc3$coefs |>
    dplyr::mutate(
      model = "SwlCbc3"
    ),
  SwlCbc3Covid$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Covid"
    ),
  SwlCbc3Low$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Low"
    ),
  SwlCbc3CovidLow$coefs |>
    dplyr::mutate(
      model = "SwlCbc3CovidLow"
    )
) |>
  dplyr::filter(coef.type == "inter_expo") |>
  dplyr::mutate(
    model_type = model |>
      stringi::stri_sub(from = 7) |>
      factor(
        levels = c("3", "3Covid", "3Low", "3CovidLow"),
        labels = c(
          "all",
          "all + post-2019",
          "low/lower-middle",
          "low/lower-middle + post-2019"
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
  filename = file.path("Output", "figure-s4.jpg"),
  plot = p_est_covid,
  units = "cm",
  width = 24,
  height = 17,
  dpi = 300
)
