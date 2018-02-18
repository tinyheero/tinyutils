# tinyutils

[![Anaconda-Server Badge](https://anaconda.org/fongchun/r-tinyutils/badges/version.svg)](https://anaconda.org/fongchun/r-tinyutils)

R package with utility functions to help with data analysis

# How to Install

The preferred way to install this package is through conda:

```
conda install -c fongchun r-tinyutils
```

Alternatively, you can install this through use devtools:

```r
devtools::install_github("tinyheero/tinyutils", upgrade_dependencies = FALSE)
```

Some functions require some bioconductor packages in order to work. As this package is not part of bioconductor, devtools will not automatically install it for you ([see this thread for more details](https://github.com/hadley/devtools/issues/700)). You will have to install them manually:

```r
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("GenomicRanges")
biocLite("Rsamtools")
```

You can also install this through bioconda: 

```r
conda install r-devtools bioconductor-genomicranges bioconductor-rsamtools
devtools::install_github("tinyheero/tinyutils", upgrade_dependencies = FALSE)
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
