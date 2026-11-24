# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_4_load-country-classification

# Script Description: Load World Bank classification into high, upper middle,
#  lower middle, and low income countries:
#  - Load: Load and process the downloaded data
#  - Repeat GBR values for NIR: Use values for Great Britain for Northern
#    Ireland as they are treated separately by the World Values Survey
#  - Add values before 1987: Countries with surveys before 1987 need to be
#    filled in manually for pre-1987 years, because the World Bank data starts
#    at 1987
#  - Add values for Serbia and Montenegro before 2006: Serbia and Montenegro
#    have World Values Survey data available before 2006, and we used
#    Yugoslavian data to fill in the gaps
#  - Add 1990 value to Russia: Russia has World Values Survey data available for
#    1990 and we filled that value in with Soviet data
#  - Add Venezuela in 2021: Venezuela has World Values Survey data available for
#    2021, but the value is missing and we filled it in manually

# Load --------------------------------------------------------------------

country_classification <- readr::read_csv(
  file.path("Input", "OGHIST.csv"),
  locale = readr::locale(encoding = "ISO-8859-1")
) |>
  tidyr::pivot_longer(
    cols = `1987`:`2024`,
    values_to = "class",
    names_to = "year"
  ) |>
  dplyr::mutate(
    class = class |>
      dplyr::replace_values(
        "LM*" ~ "LM",
        ".." ~ NA
      ),
    class = factor(
      class,
      levels = c("L", "LM", "UM", "H"),
      labels = c("low", "lower-middle", "upper-middle", "high")
    ),
    class_bin = forcats::fct_collapse(
      class,
      LLMIC = c("low", "lower-middle"),
      UMHIC = c("upper-middle", "high")
    ),
    year = as.numeric(year)
  ) |>
  dplyr::select(
    c = country_short,
    country = country_long,
    year,
    class,
    class_bin
  )

# Repeat GBR values for NIR -----------------------------------------------

nir <- country_classification |>
  dplyr::filter(c == "GBR") |>
  dplyr::mutate(
    c = "NIR",
    country = "Northern Ireland"
  )

country_classification <- country_classification |> rbind(nir)
rm(nir)

# Add values before 1987 --------------------------------------------------
## First classifications are available in 1987

## Australia 1981 (high from 1987 onwards)
aus <- data.frame(
  c = "AUS",
  country = "Australia",
  year = 1981,
  class = "high" |>
    factor(levels = c("low", "lower-middle", "upper-middle", "high")),
  class_bin = "UMHIC" |>
    factor(levels = c("LLMIC", "UMHIC"))
)

country_classification <- country_classification |> rbind(aus)
rm(aus)

## Japan 1981 (high from 1987 onwards)
jpn <- data.frame(
  c = "JPN",
  country = "Japan",
  year = 1981,
  class = "high" |>
    factor(levels = c("low", "lower-middle", "upper-middle", "high")),
  class_bin = "UMHIC" |>
    factor(levels = c("LLMIC", "UMHIC"))
)

country_classification <- country_classification |> rbind(jpn)
rm(jpn)

## Mexico 1981 (lower-middle from 87-89, then upper-middle)
mex <- data.frame(
  c = "MEX",
  country = "Mexico",
  year = 1981,
  class = "lower-middle" |>
    factor(levels = c("low", "lower-middle", "upper-middle", "high")),
  class_bin = "LLMIC" |>
    factor(levels = c("LLMIC", "UMHIC"))
)

country_classification <- country_classification |> rbind(mex)
rm(mex)

## South Africa 1982 (lower-middle in 87, then upper-middle with some switching)
zaf <- data.frame(
  c = "ZAF",
  country = "South Africa",
  year = 1982,
  class = "lower-middle" |>
    factor(levels = c("low", "lower-middle", "upper-middle", "high")),
  class_bin = "LLMIC" |>
    factor(levels = c("LLMIC", "UMHIC"))
)

country_classification <- country_classification |> rbind(zaf)
rm(zaf)

## Argentina 1984 (upper-middle in 87-88, lower-middle in 89-90, then back to
##  upper-middle)
arg <- data.frame(
  c = "ARG",
  country = "Argentina",
  year = 1984,
  class = "upper-middle" |>
    factor(levels = c("low", "lower-middle", "upper-middle", "high")),
  class_bin = "UMHIC" |>
    factor(levels = c("LLMIC", "UMHIC"))
)

country_classification <- country_classification |> rbind(arg)
rm(arg)

# Add values for Serbia and Montenegro before 2006 ------------------------

for (i in 1987:1991) {
  country_classification$class[
    country_classification$c == "SRB" & country_classification$year == i
  ] <-
    country_classification$class[
      country_classification$c == "YUGf" & country_classification$year == i
    ]

  country_classification$class_bin[
    country_classification$c == "SRB" & country_classification$year == i
  ] <-
    country_classification$class_bin[
      country_classification$c == "YUGf" & country_classification$year == i
    ]

  country_classification$class[
    country_classification$c == "MNE" & country_classification$year == i
  ] <-
    country_classification$class[
      country_classification$c == "YUGf" & country_classification$year == i
    ]

  country_classification$class_bin[
    country_classification$c == "MNE" & country_classification$year == i
  ] <-
    country_classification$class_bin[
      country_classification$c == "YUGf" & country_classification$year == i
    ]
}
rm(i)

for (i in 1992:2005) {
  country_classification$class[
    country_classification$c == "SRB" & country_classification$year == i
  ] <-
    country_classification$class[
      country_classification$c == "YUG" & country_classification$year == i
    ]

  country_classification$class_bin[
    country_classification$c == "SRB" & country_classification$year == i
  ] <-
    country_classification$class_bin[
      country_classification$c == "YUG" & country_classification$year == i
    ]

  country_classification$class[
    country_classification$c == "MNE" & country_classification$year == i
  ] <-
    country_classification$class[
      country_classification$c == "YUG" & country_classification$year == i
    ]

  country_classification$class_bin[
    country_classification$c == "MNE" & country_classification$year == i
  ] <-
    country_classification$class_bin[
      country_classification$c == "YUG" & country_classification$year == i
    ]
}
rm(i)

# Add 1990 value to Russia ------------------------------------------------

country_classification$class[
  country_classification$c == "RUS" & country_classification$year == 1990
] <-
  country_classification$class[
    country_classification$c == "SUN" & country_classification$year == 1990
  ]

country_classification$class_bin[
  country_classification$c == "RUS" & country_classification$year == 1990
] <-
  country_classification$class_bin[
    country_classification$c == "SUN" & country_classification$year == 1990
  ]

# Add Venezuela in 2021 ---------------------------------------------------
## upper-middle from 2015-2019 and missing after

country_classification$class[
  country_classification$c == "VEN" & country_classification$year == 2021
] <- "upper-middle"
country_classification$class_bin[
  country_classification$c == "VEN" & country_classification$year == 2021
] <- "UMHIC"
