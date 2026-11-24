# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_2_descriptives

# Script Description: Create Table 1, Figure 1, Figure S1, Table S1, Figure S2
#  - Table 1: Descriptive statistics by individual participants. The code
#     produces two .docx documents, one for self-rated health (srh), and one for
#     life satisfaction (ls), that have been merged manually in MS Word.
#  - Figure 1: Combinations of trade union density and collective bargaining
#     coverage
#  - Figure S1: Flowchart. The flowchart was created from a Graphviz file in
#     folder "Graph".
#  - Table S1: Descriptives by country and wave
#  - Figure S2: Time between survey and source of corresponding trade union
#     density and collective bargaining coverage values

# Table 1 ----------------------------------------------------------------

## Self-rated health ----

d_srh |>
  dplyr::select(
    srh,
    tud_original,
    cbc_original,
    gender,
    age_group,
    job,
    tu_member,
    gini_original,
    v2x_libdem_original,
    empl_stat
  ) |>
  dplyr::mutate(
    tud_original = tud_original * 100,
    cbc_original = cbc_original * 100,
    v2x_libdem_original = v2x_libdem_original * 100,
    empl_stat = empl_stat |>
      forcats::fct_recode(
        `Full-time employed` = "Full time",
        `Part-time employed` = "Part time",
        `Self-employed` = "Self employed"
      ) |>
      forcats::fct_drop()
  ) |>
  gtsummary::tbl_summary(
    include = c(
      srh,
      gender,
      age_group,
      job,
      empl_stat,
      tu_member,
      tud_original,
      cbc_original,
      v2x_libdem_original,
      gini_original
    ),
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})"
    ),
    digits = ~ list(
      p_miss = 1,
      sd = 1,
      p = 1
    ),
    label = list(
      srh = "Self-rated health",
      gender = "Female",
      age_group = "Age group",
      job = "Occupation",
      empl_stat = "Employment status",
      tu_member = "Trade union membership",
      tud_original = "Trade union density",
      cbc_original = "Collective bargaining coverage",
      v2x_libdem_original = "Democracy index",
      gini_original = "Gini index"
    )
  ) |>
  gtsummary::add_n(statistic = "{N_miss} ({p_miss}%)") |>
  gtsummary::as_flex_table() |>
  flextable::save_as_docx(path = file.path("Output", "table-1-srh.docx"))

## Life satisfaction ----

d_swl |>
  dplyr::select(
    swl,
    tud_original,
    cbc_original,
    gender,
    age_group,
    job,
    tu_member,
    gini_original,
    v2x_libdem_original,
    empl_stat
  ) |>
  dplyr::mutate(
    tud_original = tud_original * 100,
    cbc_original = cbc_original * 100,
    v2x_libdem_original = v2x_libdem_original * 100,
    empl_stat = empl_stat |>
      forcats::fct_recode(
        `Full-time employed` = "Full time",
        `Part-time employed` = "Part time",
        `Self-employed` = "Self employed"
      ) |>
      forcats::fct_drop()
  ) |>
  gtsummary::tbl_summary(
    include = c(
      swl,
      gender,
      age_group,
      job,
      empl_stat,
      tu_member,
      tud_original,
      cbc_original,
      v2x_libdem_original,
      gini_original
    ),
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})"
    ),
    digits = ~ list(
      p_miss = 1,
      sd = 1,
      p = 1
    ),
    label = list(
      swl = "Life satisfaction",
      gender = "Female",
      age_group = "Age group",
      job = "Occupation",
      empl_stat = "Employment status",
      tu_member = "Trade union membership",
      tud_original = "Trade union density",
      cbc_original = "Collective bargaining coverage",
      v2x_libdem_original = "Democracy index",
      gini_original = "Gini index"
    )
  ) |>
  gtsummary::add_n(statistic = "{N_miss} ({p_miss}%)") |>
  gtsummary::as_flex_table() |>
  flextable::save_as_docx(path = file.path("Output", "table-1-ls.docx"))

# Figure S1 --------------------------------------------------------------

file.path("Graph", "flowchart.gv") |>
  DiagrammeR::grViz() |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_png(
    file = file.path("Output", "figure-s1.png"),
    width = 2000,
    height = 2086
  )

# Table S1 ---------------------------------------------------------------

## Basic table ----
desc_c <- d_srh |>
  dplyr::select(survey, c, country, year, wave, class) |>
  plyr::count() |>
  dplyr::rename(n = freq) |>
  dplyr::mutate(
    in_srh = "yes"
  ) |>
  dplyr::full_join(
    d_swl |>
      dplyr::select(survey, c, country, year, wave, class) |>
      plyr::count() |>
      dplyr::rename(n = freq) |>
      dplyr::mutate(
        in_swl = "yes"
      ),
    by = c("survey", "c", "country", "year", "wave", "n", "class")
  ) |>
  tidyr::replace_na(list(in_srh = "NO", in_swl = "NO")) |>
  dplyr::arrange(country, wave, year) |>
  dplyr::mutate(
    survey = as.character(survey),
    income = dplyr::case_when(
      class == "low" ~ "L",
      class == "lower-middle" ~ "LM",
      class == "upper-middle" ~ "UM",
      class == "high" ~ "H"
    )
  )

desc_c$survey[desc_c$survey == "COL3" & desc_c$year == 1997] <- "COL3 (1997)"
desc_c$survey[desc_c$survey == "COL3" & desc_c$year == 1998] <- "COL3 (1998)"

desc_c <- desc_c |>
  dplyr::mutate(
    wave = paste0(
      wave,
      " (",
      year,
      ")"
    ),
    n_string = as.character(n)
  )

## Merge TUD, CBC ----
desc_c <- desc_c |>
  dplyr::left_join(
    tud |> dplyr::select(c, year, tud),
    by = c("c", "year")
  ) |>
  dplyr::left_join(
    cbc |> dplyr::select(c, year, cbc),
    by = c("c", "year")
  ) |>
  dplyr::mutate(
    TUD = tud |>
      magrittr::multiply_by(100) |>
      round(0) |>
      paste0("%"),
    CBC = cbc |>
      magrittr::multiply_by(100) |>
      round(0) |>
      paste0("%")
  )

## Add TU membership ----
srh_memb_summary <- d_srh |>
  dplyr::select(c, year, tu_member) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  )

swl_memb_summary <- d_swl |>
  dplyr::select(c, year, tu_member) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  ) |>
  dplyr::filter_out(c_year %in% srh_memb_summary$c_year)

memb_summary <- vctrs::vec_rbind(
  srh_memb_summary,
  swl_memb_summary
) |>
  dplyr::mutate(
    tum = tu_member == "member"
  ) |>
  dplyr::summarise(
    c = dplyr::first(c),
    year = dplyr::first(year),
    mean_tum = mean(tum, na.rm = TRUE),
    .by = c_year
  ) |>
  dplyr::mutate(
    TUM = mean_tum |>
      magrittr::multiply_by(100) |>
      round(0) |>
      paste0("%")
  ) |>
  dplyr::select(c, year, TUM)
rm(srh_memb_summary, swl_memb_summary)

## Add proportion of self-employed total ----
srh_self_summary <- d_srh |>
  dplyr::select(c, year, empl_stat) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  )

swl_self_summary <- d_swl |>
  dplyr::select(c, year, empl_stat) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  ) |>
  dplyr::filter_out(c_year %in% srh_self_summary$c_year)

self_summary <- vctrs::vec_rbind(
  srh_self_summary,
  swl_self_summary
) |>
  dplyr::mutate(
    self = empl_stat == "Self employed"
  ) |>
  dplyr::summarise(
    c = dplyr::first(c),
    year = dplyr::first(year),
    mean_self = mean(self, na.rm = TRUE),
    .by = c_year
  ) |>
  dplyr::mutate(
    Self = mean_self |>
      magrittr::multiply_by(100) |>
      round(0) |>
      paste0("%")
  ) |>
  dplyr::select(c, year, Self)
rm(srh_self_summary, swl_self_summary)

## Add proportion of self-employed among union members ----
srh_self_tu_summary <- d_srh |>
  dplyr::filter(tu_member == "member") |>
  dplyr::select(c, year, empl_stat) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  )

swl_self_tu_summary <- d_swl |>
  dplyr::filter(tu_member == "member") |>
  dplyr::select(c, year, empl_stat) |>
  dplyr::mutate(
    c_year = paste0(c, year)
  ) |>
  dplyr::filter_out(c_year %in% srh_self_tu_summary$c_year)

self_tu_summary <- vctrs::vec_rbind(
  srh_self_tu_summary,
  swl_self_tu_summary
) |>
  dplyr::mutate(
    self_tu = empl_stat == "Self employed"
  ) |>
  dplyr::summarise(
    c = dplyr::first(c),
    year = dplyr::first(year),
    mean_self_tu = mean(self_tu, na.rm = TRUE),
    .by = c_year
  ) |>
  dplyr::mutate(
    Self = mean_self_tu |>
      magrittr::multiply_by(100) |>
      round(0) |>
      paste0("%")
  ) |>
  dplyr::select(c, year, Self_TU = Self)
rm(srh_self_tu_summary, swl_self_tu_summary)

## TUR1990 has 0% union members and is therefore missing in self_tu_summary
self_tu_summary <- self_tu_summary |>
  vctrs::vec_rbind(
    data.frame(
      c = "TUR",
      year = 1990,
      Self_TU = "-"
    )
  ) |>
  dplyr::arrange(c, year)

## Combine ----
desc_c <- desc_c |>
  dplyr::left_join(memb_summary, by = c("c", "year")) |>
  dplyr::left_join(self_summary, by = c("c", "year")) |>
  dplyr::left_join(self_tu_summary, by = c("c", "year"))
rm(memb_summary, self_summary, self_tu_summary)

## Add country rows ----
c_unique <- desc_c$country |>
  unique() |>
  as.character()

out <- vector(mode = "list", length = length(c_unique))

for (i in seq_along(c_unique)) {
  c_temp <- c_unique[i]

  d_temp <- desc_c |>
    dplyr::filter(country == c_temp) |>
    dplyr::select(
      wave,
      income,
      n_string,
      in_srh,
      in_swl,
      TUD,
      CBC,
      TUM,
      Self,
      Self_TU
    )

  out[[i]] <- vctrs::vec_rbind(
    data.frame(
      wave = c_temp,
      income = "",
      n_string = "",
      in_srh = "",
      in_swl = "",
      TUD = "",
      CBC = "",
      TUM = "",
      Self = "",
      Self_TU = ""
    ),
    d_temp
  )
}
rm(i, c_temp, d_temp)

desc_c <- out |>
  purrr::list_rbind() |>
  dplyr::rename(
    `Wave (Year)` = wave,
    Income = income,
    n = n_string,
    `In SRH data?` = in_srh,
    `In LS data?` = in_swl,
    `%Self` = Self,
    `%Self (TU)` = Self_TU
  )
rm(out)

## Save ---
desc_c |>
  flextable::flextable() |>
  flextable::save_as_docx(path = file.path("Output", "table-s1.docx"))

# Figure S2 --------------------------------------------------------------

tc <- vctrs::vec_rbind(
  d_srh |>
    dplyr::select(c, country, year, wave),
  d_swl |>
    dplyr::select(c, country, year, wave)
) |>
  plyr::count() |>
  dplyr::select(-freq) |>
  dplyr::left_join(
    tud |>
      dplyr::select(-tud_source),
    by = c("c", "year")
  ) |>
  dplyr::left_join(
    cbc |>
      dplyr::select(-cbc_source),
    by = c("c", "year")
  ) |>
  dplyr::left_join(
    country_classification |>
      dplyr::select(-class_bin, -country),
    by = c("c", "year")
  ) |>
  dplyr::mutate(
    tud_year_diff = tud_year_diff * -1,
    cbc_year_diff = cbc_year_diff * -1,
    is_tud_diff_0 = tud_year_diff == 0,
    is_cbc_diff_0 = cbc_year_diff == 0,
    is_both_diff_0 = is_tud_diff_0 & is_cbc_diff_0
  )

tc_class <- tc |>
  dplyr::group_by(class) |>
  dplyr::select(tud_year_diff, cbc_year_diff) |>
  plyr::count() |>
  dplyr::ungroup()

p_tc_class <- tc_class |>
  ggplot2::ggplot(ggplot2::aes(y = tud_year_diff, x = cbc_year_diff)) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "16") +
  ggplot2::geom_vline(xintercept = 0, linetype = "16") +
  ggplot2::geom_hline(yintercept = 0, linetype = "16") +
  ggplot2::geom_point(ggplot2::aes(color = freq), size = 2) +
  ggside::geom_xsidehistogram(
    data = tc,
    binwidth = 1
  ) +
  ggside::geom_ysidehistogram(
    data = tc,
    binwidth = 1
  ) +
  ggside::geom_xsidevline(xintercept = 0, linetype = "16") +
  ggside::geom_ysidehline(yintercept = 0, linetype = "16") +
  ggplot2::scale_color_distiller(
    palette = "Spectral",
    transform = "sqrt",
    breaks = c(1, 3, 7, 15, 30)
  ) +
  ggside::scale_xsidey_continuous(breaks = c(0, 20, 40), limits = c(0, 47)) +
  ggside::scale_ysidex_continuous(breaks = c(0, 20, 40), limits = c(0, 47)) +
  ggplot2::theme_bw() +
  ggside::theme_ggside_bw() +
  ggplot2::theme(
    ggside.panel.scale.y = 0.16,
    ggside.panel.scale.x = 0.19,
    ggside.panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_legend(position = "bottom") +
  ggplot2::labs(
    y = "Time in years between the survey and the source of its TUD value",
    x = "Time in years between the survey and the source of its CBC value",
    color = "Frequency"
  ) +
  ggplot2::facet_wrap(~class)

ggplot2::ggsave(
  filename = file.path("Output", "figure-s2.jpg"),
  plot = p_tc_class,
  units = "cm",
  width = 21,
  height = 20,
  dpi = 300
)
rm(tc_class, p_tc_class)

# Figure 1 ---------------------------------------------------------------

p_corr <- tc |>
  ggplot2::ggplot(ggplot2::aes(x = cbc, y = tud)) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "16") +
  ggplot2::geom_point(
    ggplot2::aes(color = class, shape = is_both_diff_0),
    alpha = 0.8,
    size = 2
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "blue",
      "deepskyblue1",
      "darkorange",
      "red"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme_sub_legend(position = "bottom") +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::labs(
    x = "Collective bargaining coverage",
    y = "Trade union density",
    color = "Income",
    shape = "Survey year values"
  )

ggplot2::ggsave(
  filename = file.path("Output", "figure-1.jpg"),
  plot = p_corr,
  units = "cm",
  width = 20,
  height = 22,
  dpi = 300
)
rm(tc, p_corr)
