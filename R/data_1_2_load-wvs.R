# Title -------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: data_1_2_load-wvs

# Script Description: Load and process World Values Survey data:
# - Load and select variables: Load the dataset from cache if available.
#   Otherwise, load the data from file and select the variables required for
#   analysis. It seems like the file was initially in SAS or Stata format
#   before it has been transformed to .rds. haven::zap_label/s() removes
#   remaining SAS or Stata structures.
# - Filter: Exclude surveys that did not ask for certain variables
# - Process variables: Create new variables from existing ones
# - Join country label: Add the full name of countries to the dataset
# - Self-rated health: Create the dataset that is used for models with outcome
#   self-rated health
# - Life satisfaction: Create the dataset that is used for models with outcome
#   life satisfaction
# - Remove d: Remove the combined dataset and only keep the datasets for
#   self-rated health and life satisfaction

# Load and select variables -----------------------------------------------

if (cache$exists("wvs_data")) {
  d <- cache$get("wvs_data")
} else {
  d <- readRDS(
    file.path("Input", "WVS_Time_Series_1981-2022_rds_v5_0.rds")
  ) |>
    dplyr::select(
      wave = S002VS,
      c = COUNTRY_ALPHA,
      year = S020,
      S022,
      S023,
      A009,
      A067,
      A101,
      X001,
      X003,
      X028,
      X036,
      X036E,
      A170,
      X025R,
      X047R_WVS
    ) |>
    haven::zap_label() |>
    dplyr::mutate(
      wave = haven::zap_labels(wave),
      year = haven::zap_labels(year),
      A009 = haven::zap_labels(A009),
      A067 = haven::zap_labels(A067),
      A101 = haven::zap_labels(A101),
      X028 = haven::zap_labels(X028),
      X001 = haven::zap_labels(X001),
      X003 = haven::zap_labels(X003),
      X036 = haven::zap_labels(X036),
      X036E = haven::zap_labels(X036E),
      A170 = haven::zap_labels(A170),
      X025R = haven::zap_labels(X025R),
      X047R_WVS = haven::zap_labels(X047R_WVS)
    )

  cache$set("wvs_data", d)
}

n_0 <- nrow(d)

# Filter ------------------------------------------------------------------

## Exclude waves that did not ask for union membership
cwave_excluded_2 <- d |>
  dplyr::filter_out(A067 != -4 | A101 != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d <- d |>
  dplyr::filter(A067 != -4 | A101 != -4)

n_2 <- nrow(d)

## Exclude waves that did not ask for occupation
cwave_excluded_3 <- d |>
  dplyr::filter_out(X036 != -4 | X036E != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d <- d |>
  dplyr::filter(X036 != -4 | X036E != -4)

n_3 <- nrow(d)

## Exclude waves that did not ask for employment status
cwave_excluded_4 <- d |>
  dplyr::filter_out(X028 != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d <- d |>
  dplyr::filter(X028 != -4)

n_4 <- nrow(d)

## Exclude waves that did not ask for gender
cwave_excluded_5 <- d |>
  dplyr::filter_out(X001 != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d <- d |>
  dplyr::filter(X001 != -4)

n_5 <- nrow(d)

# Process variables -------------------------------------------------------

d <- d |>
  dplyr::mutate(
    job = dplyr::case_when(
      X036 == -5 ~ NA,
      X036 == -3 ~ NA,
      X036 == -2 ~ NA,
      X036 == -1 ~ NA,
      X036E == -5 ~ NA,
      X036E == -3 ~ NA,
      X036E == -2 ~ NA,
      X036E == -1 ~ NA,
      X036 == 13 ~ "Primarily non-manual",
      X036 == 16 ~ "Primarily non-manual",
      X036 == 21 ~ "Primarily non-manual",
      X036 == 22 ~ "Primarily non-manual",
      X036 == 23 ~ "Primarily non-manual",
      X036 == 24 ~ "Primarily non-manual",
      X036 == 25 ~ "Primarily non-manual",
      X036 == 31 ~ "Primarily manual",
      X036 == 32 ~ "Primarily manual",
      X036 == 33 ~ "Primarily manual",
      X036 == 34 ~ "Primarily manual",
      X036 == 41 ~ "Agricultural",
      X036 == 42 ~ "Agricultural",
      X036 == 51 ~ "Other",
      X036 == 61 ~ "None",
      X036 == 81 ~ "Other",
      X036E == 0 ~ "None",
      X036E == 1 ~ "Primarily non-manual",
      X036E == 2 ~ "Primarily non-manual",
      X036E == 3 ~ "Primarily non-manual",
      X036E == 4 ~ "Primarily non-manual",
      X036E == 5 ~ "Primarily manual",
      X036E == 6 ~ "Primarily manual",
      X036E == 7 ~ "Primarily manual",
      X036E == 8 ~ "Primarily manual",
      X036E == 9 ~ "Agricultural",
      X036E == 10 ~ "Agricultural",
      X036E == 11 ~ "Other"
    ),
    job2 = factor(
      job,
      levels = c(
        "Primarily non-manual",
        "Primarily manual",
        "Agricultural",
        "Other",
        "None"
      )
    ),
    job = factor(
      job,
      levels = c(
        "Primarily non-manual",
        "Primarily manual",
        "Agricultural"
      )
    ),
    empl_stat = factor(
      X028,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8),
      labels = c(
        "Full time",
        "Part time",
        "Self employed",
        "Retired",
        "Housewife",
        "Students",
        "Unemployed",
        "Other"
      )
    ),
    tu_member = dplyr::case_when(
      A067 == 0 & A101 == -4 ~ 0,
      A067 == 1 & A101 == -4 ~ 1,
      A101 == 0 ~ 0,
      A101 == 1 ~ 1,
      A101 == 2 ~ 1
    ) |>
      factor(levels = c(0, 1), labels = c("nonmember", "member")),
    gender = factor(X001, levels = c(1, 2), labels = c("male", "female")),
    age = X003 |>
      dplyr::replace_values(
        c(-5, -4, -2, -1) ~ NA
      ),
    age_group = dplyr::case_when(
      age < 20 ~ "<20",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 ~ "60+"
    ) |>
      factor(
        levels = c("<20", "20-29", "30-39", "40-49", "50-59", "60+")
      ),
    survey = paste0(c, wave) |>
      as.factor(),
    decades_since_start = (year - min(year)) / 10
  )

# Testing
# d |>
#   dplyr::select(A009, health_4cat, health_4cat_fac) |>
#   plyr::count()

# d |>
#   dplyr::select(X036, X036E, job) |>
#   plyr::count()

# d |>
#   dplyr::select(X028, empl_stat) |>
#   plyr::count()

# d |>
#   dplyr::select(A067, A101, tu_member) |>
#   plyr::count()

# d |>
#   dplyr::select(age, age_group) |>
#   plyr::count()

# d |>
#   dplyr::select(year, decades_since_start) |>
#   plyr::count()

# Join country label ------------------------------------------------------

wvs_country_codes <- c(
  "ALB",
  "Albania",
  "AND",
  "Andorra",
  "ARG",
  "Argentina",
  "ARM",
  "Armenia",
  "AUS",
  "Australia",
  "AZE",
  "Azerbaijan",
  "BFA",
  "Burkina Faso",
  "BGD",
  "Bangladesh",
  "BGR",
  "Bulgaria",
  "BIH",
  "Bosnia and Herzegovina",
  "BLR",
  "Belarus",
  "BOL",
  "Bolivia",
  "BRA",
  "Brazil",
  "CAN",
  "Canada",
  "CHE",
  "Switzerland",
  "CHL",
  "Chile",
  "CHN",
  "China",
  "COL",
  "Colombia",
  "CYP",
  "Cyprus",
  "CZE",
  "Czechia",
  "DEU",
  "Germany",
  "DOM",
  "Dominican Republic",
  "DZA",
  "Algeria",
  "ECU",
  "Ecuador",
  "EGY",
  "Egypt",
  "ESP",
  "Spain",
  "EST",
  "Estonia",
  "ETH",
  "Ethiopia",
  "FIN",
  "Finland",
  "FRA",
  "France",
  "GBR",
  "Great Britain",
  "GEO",
  "Georgia",
  "GHA",
  "Ghana",
  "GRC",
  "Greece",
  "GTM",
  "Guatemala",
  "HKG",
  "Hong Kong SAR",
  "HRV",
  "Croatia",
  "HTI",
  "Haiti",
  "HUN",
  "Hungary",
  "IDN",
  "Indonesia",
  "IND",
  "India",
  "IRN",
  "Iran",
  "IRQ",
  "Iraq",
  "ISR",
  "Israel",
  "ITA",
  "Italy",
  "JOR",
  "Jordan",
  "JPN",
  "Japan",
  "KAZ",
  "Kazakhstan",
  "KEN",
  "Kenya",
  "KGZ",
  "Kyrgyzstan",
  "KOR",
  "South Korea",
  "KWT",
  "Kuwait",
  "LBN",
  "Lebanon",
  "LBY",
  "Libya",
  "LTU",
  "Lithuania",
  "LVA",
  "Latvia",
  "MAC",
  "Macau SAR",
  "MAR",
  "Morocco",
  "MDA",
  "Moldova",
  "MDV",
  "Maldives",
  "MEX",
  "Mexico",
  "MKD",
  "North Macedonia",
  "MLI",
  "Mali",
  "MMR",
  "Myanmar",
  "MNE",
  "Montenegro",
  "MNG",
  "Mongolia",
  "MYS",
  "Malaysia",
  "NGA",
  "Nigeria",
  "NIC",
  "Nicaragua",
  "NIR",
  "North Ireland",
  "NLD",
  "Netherlands",
  "NOR",
  "Norway",
  "NZL",
  "New Zealand",
  "PAK",
  "Pakistan",
  "PER",
  "Peru",
  "PHL",
  "Philippines",
  "POL",
  "Poland",
  "PRI",
  "Puerto Rico",
  "PSE",
  "Palestine",
  "QAT",
  "Qatar",
  "ROU",
  "Romania",
  "RUS",
  "Russian Federation",
  "RWA",
  "Rwanda",
  "SAU",
  "Saudi Arabia",
  "SGP",
  "Singapore",
  "SLV",
  "El Salvador",
  "SRB",
  "Serbia",
  "SVK",
  "Slovakia",
  "SVN",
  "Slovenia",
  "SWE",
  "Sweden",
  "THA",
  "Thailand",
  "TJK",
  "Tajikistan",
  "TTO",
  "Trinidad and Tobago",
  "TUN",
  "Tunisia",
  "TUR",
  "Turkey",
  "TWN",
  "Taiwan ROC",
  "TZA",
  "Tanzania",
  "UGA",
  "Uganda",
  "UKR",
  "Ukraine",
  "URY",
  "Uruguay",
  "USA",
  "United States",
  "UZB",
  "Uzbekistan",
  "VEN",
  "Venezuela",
  "VNM",
  "Vietnam",
  "YEM",
  "Yemen",
  "ZAF",
  "South Africa",
  "ZMB",
  "Zambia",
  "ZWE",
  "Zimbabwe"
) |>
  matrix(ncol = 2, byrow = TRUE) |>
  as.data.frame() |>
  magrittr::set_colnames(c("c", "country"))

d <- d |>
  dplyr::left_join(wvs_country_codes, by = "c") |>
  dplyr::mutate(
    country = as.factor(country)
  )
rm(wvs_country_codes)

# Self-rated health ------------------------------------------------------

## Remove life satisfaction
d_srh <- d |>
  dplyr::select(-A170)

## Exclude waves that did not ask for self-rated health
cwave_excluded_10_srh <- d_srh |>
  dplyr::filter_out(A009 != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d_srh <- d_srh |>
  dplyr::filter(A009 != -4)

n_10_srh <- nrow(d_srh)

## Process variables
d_srh <- d_srh |>
  dplyr::mutate(
    health_4cat = A009 |>
      dplyr::replace_values(
        c(-5, -2, -1) ~ NA,
        c(4, 5) ~ 4 # Combine poor and very poor
      ),
    srh = factor(
      health_4cat,
      levels = c(1, 2, 3, 4),
      labels = c("Very good", "Good", "Fair", "Poor or very poor"),
      ordered = TRUE
    )
  )

# Life satisfaction ------------------------------------------------------

## Remove self-rated health
d_swl <- d |>
  dplyr::select(-A009)

## Exclude waves that did not ask for life satisfaction
cwave_excluded_10_swl <- d_swl |>
  dplyr::filter_out(A170 != -4) |>
  dplyr::select(c, wave) |>
  dplyr::mutate(
    cwave = paste0(c, wave)
  ) |>
  magrittr::extract2("cwave") |>
  unique() |>
  stringi::stri_c(collapse = ", ")

d_swl <- d_swl |>
  dplyr::filter(A170 != -4)

n_10_swl <- nrow(d_swl)

## Process variables
d_swl <- d_swl |>
  dplyr::mutate(
    swl = A170 |>
      dplyr::recode_values(
        -5 ~ NA_character_,
        -2 ~ NA_character_,
        -1 ~ NA_character_,
        1 ~ "very low",
        2 ~ "very low",
        3 ~ "low",
        4 ~ "low",
        5 ~ "medium",
        6 ~ "medium",
        7 ~ "high",
        8 ~ "high",
        9 ~ "very high",
        10 ~ "very high"
      ) |>
      factor(
        levels = c("very high", "high", "medium", "low", "very low"),
        ordered = TRUE
      )
  )

# Remove d ---------------------------------------------------------------

rm(d)
