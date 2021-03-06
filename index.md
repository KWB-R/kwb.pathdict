[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/o1y4af5k857y1s32/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-pathdict/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.pathdict.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.pathdict)
[![codecov](https://codecov.io/github/KWB-R/kwb.pathdict/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.pathdict)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.pathdict)]()

This package provides functions to work with
what I call path dictionaries. Path dictionaries are lists defining
file and folder paths. In order not to repeat sub-paths, placeholders
can be used. The package provides functions to find duplicated
sub-paths and to define placeholders accordingly.

## Installation

For more details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Option: specify GitHub Personal Access Token (GITHUB_PAT)
### see: https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat
### why this might be important for you!

#Sys.setenv(GITHUB_PAT = "mysecret_access_token")

if (!require("remotes")) {
install.packages("remotes", repos = "https://cloud.r-project.org")
}

### Temporal workaround to due bug in latest CRAN of R package remotes v2.0.2
### on Windows(for details: see https://github.com/r-lib/remotes/issues/248)

remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.pathdict")
```
