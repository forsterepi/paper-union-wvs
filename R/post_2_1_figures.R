# Title ------------------------------------------------------------------
# Project: paper-union-wvs
# Script Title: post_2_1_figures

# Script Description: Create Figure 2, Figure 3, Table S2, and Results section
#  predictions
# - Figure 2: Model estimates
# - Figure 3: Predictions
# - Table S2: Values plotted in Figure 2
# - Results section predictions: Collect numbers useful for describing Figure 3
#   in the Results section of the article

# Figure 3 ---------------------------------------------------------------

p_pred_low_all <- patchwork::wrap_plots(
  p_srh_tud_pred_low,
  p_swl_tud_pred_low,
  p_srh_cbc_pred_low,
  p_swl_cbc_pred_low,
  nrow = 1
)

ggplot2::ggsave(
  filename = file.path("Output", "figure-3.jpg"),
  plot = p_pred_low_all,
  units = "cm",
  width = 48,
  height = 29,
  dpi = 300
)

# Figure 2 ---------------------------------------------------------------

d_est <- vctrs::vec_rbind(
  SrhTud0$coefs |>
    dplyr::mutate(
      model = "SrhTud0"
    ),
  SrhTud1$coefs |>
    dplyr::mutate(
      model = "SrhTud1"
    ),
  SrhTud2$coefs |>
    dplyr::mutate(
      model = "SrhTud2"
    ),
  SrhTud3$coefs |>
    dplyr::mutate(
      model = "SrhTud3"
    ),
  SrhTud3Low$coefs |>
    dplyr::mutate(
      model = "SrhTud3Low"
    ),
  SwlTud0$coefs |>
    dplyr::mutate(
      model = "SwlTud0"
    ),
  SwlTud1$coefs |>
    dplyr::mutate(
      model = "SwlTud1"
    ),
  SwlTud2$coefs |>
    dplyr::mutate(
      model = "SwlTud2"
    ),
  SwlTud3$coefs |>
    dplyr::mutate(
      model = "SwlTud3"
    ),
  SwlTud3Low$coefs |>
    dplyr::mutate(
      model = "SwlTud3Low"
    ),
  SrhCbc0$coefs |>
    dplyr::mutate(
      model = "SrhCbc0"
    ),
  SrhCbc1$coefs |>
    dplyr::mutate(
      model = "SrhCbc1"
    ),
  SrhCbc2$coefs |>
    dplyr::mutate(
      model = "SrhCbc2"
    ),
  SrhCbc3$coefs |>
    dplyr::mutate(
      model = "SrhCbc3"
    ),
  SrhCbc3Low$coefs |>
    dplyr::mutate(
      model = "SrhCbc3Low"
    ),
  SwlCbc0$coefs |>
    dplyr::mutate(
      model = "SwlCbc0"
    ),
  SwlCbc1$coefs |>
    dplyr::mutate(
      model = "SwlCbc1"
    ),
  SwlCbc2$coefs |>
    dplyr::mutate(
      model = "SwlCbc2"
    ),
  SwlCbc3$coefs |>
    dplyr::mutate(
      model = "SwlCbc3"
    ),
  SwlCbc3Low$coefs |>
    dplyr::mutate(
      model = "SwlCbc3Low"
    )
) |>
  dplyr::filter(coef.type == "inter_expo") |>
  dplyr::mutate(
    model_type = model |>
      stringi::stri_sub(from = 7) |>
      factor(
        levels = c("0", "1", "2", "3", "3Low"),
        labels = c(
          "expo*year",
          "+ indv-lvl",
          "+ expo-related",
          "+ country-lvl",
          "low/lower-middle only"
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
  )

p_est <- d_est |>
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
  ggplot2::scale_color_manual(
    values = c(
      "#cccccc",
      "#888888",
      "#555555",
      "#000000",
      "blue"
    )
  ) +
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
  filename = file.path("Output", "figure-2.jpg"),
  plot = p_est,
  units = "cm",
  width = 24,
  height = 17,
  dpi = 300
)

# Table S2 ---------------------------------------------------------------

d_est_srh <- d_est |>
  dplyr::filter(outcome == "Self-rated health") |>
  dplyr::arrange(dplyr::desc(term), dplyr::desc(model_type)) |>
  dplyr::mutate(
    est = paste0(
      estimate |>
        round(2) |>
        format(nsmall = 2),
      "\n(",
      lwr |>
        round(2) |>
        format(nsmall = 2),
      "-",
      upr |>
        round(2) |>
        format(nsmall = 2),
      ")"
    )
  ) |>
  dplyr::select(Variable = term, Model = model_type, `Self-rated health` = est)

d_est_swl <- d_est |>
  dplyr::filter(outcome == "Life satisfaction") |>
  dplyr::arrange(dplyr::desc(term), dplyr::desc(model_type)) |>
  dplyr::mutate(
    est = paste0(
      estimate |>
        round(2) |>
        format(nsmall = 2),
      " (",
      lwr |>
        round(2) |>
        format(nsmall = 2),
      "-",
      upr |>
        round(2) |>
        format(nsmall = 2),
      ")"
    )
  ) |>
  dplyr::select(Variable = term, Model = model_type, `Life satisfaction` = est)

d_est_srh |>
  dplyr::left_join(d_est_swl, by = c("Variable", "Model")) |>
  flextable::flextable() |>
  flextable::save_as_docx(path = file.path("Output", "table-s2.docx"))

# Results section predictions --------------------------------------------

vctrs::vec_rbind(
  pred_values_srh_tud,
  pred_values_swl_tud,
  pred_values_srh_cbc,
  pred_values_swl_cbc
) |>
  flextable::flextable() |>
  flextable::save_as_docx(
    path = file.path("Output", "result-section-predictions.docx")
  )

rm(
  pred_values_srh_tud,
  pred_values_swl_tud,
  pred_values_srh_cbc,
  pred_values_swl_cbc
)
