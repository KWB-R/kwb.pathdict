remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.pkgbuild")

# Set the name for your new package
package <- "kwb.pathdict"

# Set the path to your new package
pkg_dir <- getwd()

stopifnot(basename(pkg_dir) == package)

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

name <- "Hauke Sonnenberg"

author <- list(
  name = name,
  orcid = unname(kwb.orcid::get_kwb_orcids()[name]),
  url = "https://github.com/hsonne"
)

description <- list(
  name = package,
  title = "Functions to Work with Path Dictionaries",
  desc  = paste(
    collapse = "\n  ",
    "This package provides functions to work with what I call path ",
    "dictionaries. Path dictionaries are lists defining file and folder paths.",
    "In order not to repeat sub-paths, placeholders can be used. The package",
    "provides functions to find duplicated sub-paths and to define placeholders",
    "accordingly."
  )
)

setwd(pkg_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

