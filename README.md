# tinyutils

R package with utility functions to help with data analysis

To install this package using devtools:

```{r}
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
