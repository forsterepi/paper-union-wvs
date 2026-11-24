# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_2_2_pred-lower-middle

# Script Description: Create prediction plots for lower-middle income countries.
#  This script creates Figure S3.
# - Plot predictions SrhTud3: Create sub-plot for models investigating self-
#   rated health ~ trade union density
# - Plot predictions SwlTud3: Create sub-plot for models investigating life
#   satisfaction ~ trade union density
# - Plot predictions SrhCbc3: Create sub-plot for models investigating self-
#   rated health ~ collective bargaining coverage
# - Plot predictions SwlCbc3: Create sub-plot for models investigating life
#   satisfaction ~ collective bargaining coverage
# - Combine: Combine sub-plots

# Plot predictions SrhTud3 -----------------------------------------------

axis_exposure <- "Trade union density"
axis_outcome <- "Expected self-rated health (higher is worse)"

## Models with all countries ----
pred_low <- SrhTud3$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Models with low/lower-middle only ----
pred_stratified_low <- SrhTud3Low$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Plot ----
p_srh_tud_pred_lowermiddle <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = tud, y = mean_sampled, group = c),
    colour = "grey",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = tud, y = mean_sampled, group = c),
    color = "blue",
    linetype = "dotted",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = tud,
      ymin = q025_sampled,
      ymax = q975_sampled,
      group = c
    ),
    color = "blue"
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
  ggplot2::labs(
    x = axis_exposure,
    y = axis_outcome
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 13)) +
  ggplot2::facet_wrap(~wave, ncol = 1)

## Clean
rm(
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low
)

# Plot predictions SwlTud3 -----------------------------------------------

axis_exposure <- "Trade union density"
axis_outcome <- "Expected life satisfaction (higher is worse)"

## Models with all countries ----
pred_low <- SwlTud3$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Models with low/lower-middle only ----
pred_stratified_low <- SwlTud3Low$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Plot ----
p_swl_tud_pred_lowermiddle <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = tud, y = mean_sampled, group = c),
    colour = "grey",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = tud, y = mean_sampled, group = c),
    color = "blue",
    linetype = "dotted",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = tud,
      ymin = q025_sampled,
      ymax = q975_sampled,
      group = c
    ),
    color = "blue"
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
  ggplot2::labs(
    x = axis_exposure,
    y = axis_outcome
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 13)) +
  ggplot2::facet_wrap(~wave, ncol = 1)

## Clean
rm(
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low
)

# Plot predictions SrhCbc3 -----------------------------------------------

axis_exposure <- "Collective bargaining coverage"
axis_outcome <- "Expected self-rated health (higher is worse)"

## Models with all countries ----
pred_low <- SrhCbc3$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Models with low/lower-middle only ----
pred_stratified_low <- SrhCbc3Low$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Plot ----
p_srh_cbc_pred_lowermiddle <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = cbc, y = mean_sampled, group = c),
    colour = "grey",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = cbc, y = mean_sampled, group = c),
    color = "blue",
    linetype = "dotted",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = cbc,
      ymin = q025_sampled,
      ymax = q975_sampled,
      group = c
    ),
    color = "blue"
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
  ggplot2::labs(
    x = axis_exposure,
    y = axis_outcome
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 13)) +
  ggplot2::facet_wrap(~wave, ncol = 1)

## Clean
rm(
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low
)

# Plot predictions SwlCbc3 -----------------------------------------------

axis_exposure <- "Collective bargaining coverage"
axis_outcome <- "Expected life satisfaction (higher is worse)"

## Models with all countries ----
pred_low <- SwlCbc3$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Models with low/lower-middle only ----
pred_stratified_low <- SwlCbc3Low$pred |>
  dplyr::filter(class == "lower-middle") |>
  dplyr::mutate(
    wave = wave |>
      forcats::fct_drop() |>
      forcats::fct_recode(
        `Wave 2 (1990)` = "2",
        `Wave 3 (1995-99)` = "3",
        `Wave 4 (2001-02)` = "4",
        `Wave 5 (2006-09)` = "5",
        `Wave 7 (2018-19)` = "7"
      )
  )

## Plot ----
p_swl_cbc_pred_lowermiddle <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_low,
    mapping = ggplot2::aes(x = cbc, y = mean_sampled, group = c),
    colour = "grey",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(
    data = pred_stratified_low,
    mapping = ggplot2::aes(x = cbc, y = mean_sampled, group = c),
    color = "blue",
    linetype = "dotted",
    linewidth = 0.7
  ) +
  ggplot2::geom_linerange(
    data = pred_stratified_low,
    mapping = ggplot2::aes(
      x = cbc,
      ymin = q025_sampled,
      ymax = q975_sampled,
      group = c
    ),
    color = "blue"
  ) +
  ggplot2::coord_cartesian(ylim = c(1, nlevels(d_swl$swl))) +
  ggplot2::scale_x_continuous(
    breaks = SwlCbc3$pred$cbc |>
      unique() |>
      sort(),
    labels = SwlCbc3$pred$cbc |>
      unique() |>
      sort() |>
      magrittr::multiply_by(cbc_sd_swl) |>
      magrittr::add(cbc_mean_swl) |>
      round(3) |>
      magrittr::multiply_by(100) |>
      paste0("%")
  ) +
  ggplot2::labs(
    x = axis_exposure,
    y = axis_outcome
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 13),
    title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(size = 13)) +
  ggplot2::facet_wrap(~wave, ncol = 1)

## Clean
rm(
  axis_outcome,
  axis_exposure,
  pred_low,
  pred_stratified_low
)

# Combine ----------------------------------------------------------------

p_pred_lowermiddle_all <- patchwork::wrap_plots(
  p_srh_tud_pred_lowermiddle,
  p_swl_tud_pred_lowermiddle,
  p_srh_cbc_pred_lowermiddle,
  p_swl_cbc_pred_lowermiddle,
  nrow = 1
)

ggplot2::ggsave(
  filename = file.path("Output", "figure-s3.jpg"),
  plot = p_pred_lowermiddle_all,
  units = "cm",
  width = 48,
  height = 29,
  dpi = 300
)
