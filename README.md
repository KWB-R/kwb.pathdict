[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.pathdict?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-pathdict/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.pathdict.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.pathdict)
[![codecov](https://codecov.io/github/KWB-R/kwb.pathdict/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.pathdict)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.pathdict)]()

# kwb.pathdict

This package provides functions to work with what I call path 
dictionaries. Path dictionaries are lists defining file and folder paths. 
In order not to repeat sub-paths, placeholders can be used. The package 
provides functions to find duplicated sub-paths and to define placeholders 
accordingly.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

### Temporary workaround on Windows to fix bug in CRAN version v2.0.2
### of "remotes" (see https://github.com/r-lib/remotes/issues/248)

remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.pathdict")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.pathdict](https://kwb-r.github.io/kwb.pathdict)

Development: [https://kwb-r.github.io/kwb.pathdict/dev](https://kwb-r.github.io/kwb.pathdict/dev)
