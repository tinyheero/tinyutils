# tinyutils

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tinyutils)](http://cran.r-project.org/package=tinyutils) [![Travis-CI Build Status](https://travis-ci.org/tinyheero/tinyutils.svg?branch=master)](https://travis-ci.org/tinyheero/tinyutils)

R package with utility functions to help with data analysis

# How to Install

To install this package, use devtools:

```{r}
devtools::install_github("tinyheero/tinyutils", upgrade_dependencies = FALSE)
```

Some functions require some bioconductor packages in order to work. As this package is not part of bioconductor, devtools will not automatically install it for you ([see this thread for more details](https://github.com/hadley/devtools/issues/700)). You will have to install them manually:

```{r}
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("GenomicRanges")
biocLite("Rsamtools")
```

You can also install this through bioconda: 

```{r}
conda install r-devtools bioconductor-genomicranges bioconductor-rsamtools
devtools::install_github("tinyheero/tinyutils")
```

# Overview

To see the full list of exported functions:

```{r}
library("tinyutils")
ls("package:tinyutils")
```

A quick overview of some of the key functions:

* `plot_feature_sample_mat`: Plots a co-occurence (i.e. feature-sample) matrix plot. 

    > This function has been deprecated. It has been moved to the [cofeatureR](https://cran.r-project.org/web/packages/cofeatureR/index.html) package and renamed to `plot_cofeature_mat`.

* `make_presence_absence_mat`: Create a presence/asbence (PA) matrix 
