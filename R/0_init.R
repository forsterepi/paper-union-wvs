# Title ------------------------------------------------------------------
# Project Title: paper-union-wvs
# Script Title: 0_init

# Script Description: Project initialisation
# - Check R version and packages: Checking if at least R version 4.6.0 is used.
#   Warning if a different R version from 4.6.1 is used. Checking if all
#   required packages are installed. Warning if a different version of any
#   required package is used.
# - Check availability of function package: Checking if package
#   `functionsUnionHealth`, which contains custom functions, has been installed
#   from GitHub (see section "Further topics/Function package" in the README)
# - Initialise cache: Initialising the cache in folder "Cache" (see section
#   "Further topics/Cache" in the README)

# Check R version and packages -------------------------------------------

# Package `rlang` is used to check if all necessary packages are available.
# Therefore, we check first if `rlang` is installed.

## Check if rlang is installed
pkg_list <- installed.packages()
# (Cannot use %notin% before checking R version)
if (!("rlang" %in% as.character(pkg_list[, 1]))) {
  rm(pkg_list)
  stop("Please install package `rlang`.")
}
rm(pkg_list)

## Check R version
if (!rlang::is_installed("base (>= 4.6)")) {
  stop(
    paste0(
      "You need at least R version 4.6.0 to run this analysis, because we use",
      " `%notin%` in the analysis."
    )
  )
}

if (!rlang::is_installed("base (== 4.6.1)")) {
  warning(
    paste0(
      "For our analysis, we used R version 4.6.1, which is different from ",
      "your version. This might not be a problem, but if you run into errors ",
      "while running the code, consider using the same R versions."
    )
  )
}

## Load list of required packages
# (Misc/required-packages.rds has been created in R/0_required-packages.R)
req_pkg <- readRDS(file = file.path("Misc", "required-packages.rds"))

## Check package availability
rlang::check_installed(req_pkg$pkg)

## Check package versions
for (i in seq_len(nrow(req_pkg))) {
  if (
    !rlang::is_installed(
      pkg = req_pkg$pkg[i],
      version = req_pkg$version[i],
      compare = "=="
    )
  ) {
    warning(
      paste0(
        "For our analysis, we used version ",
        req_pkg$version[i],
        " of package `",
        req_pkg$pkg[i],
        "`. You have a different version installed. This will likely not be a ",
        "problem, but if you run into errors while running the code, consider ",
        "using the same package versions."
      )
    )
  }
}
rm(i, req_pkg)

# Check availability of function package ---------------------------------

if (!rlang::is_installed("functionsUnionHealth")) {
  stop(
    paste0(
      "Please install package `functionsUnionHealth`, which contains custom ",
      "functions created for this project. See the README for how to install ",
      "the package."
    )
  )
}

# Initialise cache -------------------------------------------------------

cache <- storr::storr_rds("Cache")
