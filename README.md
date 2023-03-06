
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grantpubcite

<!-- badges: start -->
<!-- badges: end -->

The ‘grantpubcite’ package is used to query the [NIH
Reporter](https://reporter.nih.gov/) database for funded grants and,
optionally, publications associated with grants in NIH Reporter. The
citation history of publications can discovered using
[iCite](https://icite.od.nih.gov/). The ‘grantpubcite’ package help
pages and resources make extensive use of ‘tidyverse’ concepts.

Install the development version of grantpubcite from
[github](https://github.com/mtmorgan/grantpubcite).

``` r
if (!nzchar(system.file(package = "remotes")))
    install.package("remotes", repos = "https://cran.r-project.org")
remotes::install_github("mtmorgan/grantpubcite")
```

Load the library and other packages to be used in the vignette.

``` r
library(grantpubcite)
```

See

-   [Get started](articles/grantpubcite.html) for an introduction to the
    package.
-   [Case study: ITCR](articles/case_study_itcr) for an illustration of
    package use analysing grants in the NIH Information Technology in
    Cancer Research portfolio of grants.

Please note that I am not experienced in this sort of analysis, so any
conclusions should be viewed with great skepticism.
