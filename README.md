
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
devtools::install_github("mtmorgan/grantpubcite")
```

Load the library and other packages to be used in the vignette.

``` r
library(grantpubcite)
```

See

-   [Get started](articles/grantpubcite.html) for an introduction to the
    package.
-   [Case study: ITCR](articles/case_study_itcr) for an illustration of
    package use analysing grants in the NIH Information in Technology in
    Cancer Research portfolio of grants.

Please note that I am not experienced in this sort of analysis, so any
conclusions should be viewed with great skepticism.

This site was processed using the following software:

``` r
sessionInfo()
#> R Under development (unstable) (2023-02-21 r83887)
#> Platform: aarch64-apple-darwin21.6.0 (64-bit)
#> Running under: macOS Monterey 12.6.2
#> 
#> Matrix products: default
#> BLAS:   /Users/ma38727/bin/R-devel/lib/libRblas.dylib 
#> LAPACK: /Users/ma38727/bin/R-devel/lib/libRlapack.dylib;  LAPACK version 3.11.0
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: America/New_York
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] grantpubcite_0.0.0.9003 dplyr_1.1.0            
#> 
#> loaded via a namespace (and not attached):
#>  [1] vctrs_0.5.2      httr_1.4.5       cli_3.6.0        knitr_1.42      
#>  [5] rlang_1.0.6      xfun_0.37        generics_0.1.3   jsonlite_1.8.4  
#>  [9] glue_1.6.2       rjsoncons_1.0.0  htmltools_0.5.4  hms_1.1.2       
#> [13] fansi_1.0.4      rmarkdown_2.20   evaluate_0.20    tibble_3.1.8    
#> [17] tzdb_0.3.0       ellipsis_0.3.2   fastmap_1.1.1    yaml_2.3.7      
#> [21] lifecycle_1.0.3  compiler_4.3.0   pkgconfig_2.0.3  digest_0.6.31   
#> [25] R6_2.5.1         readr_2.1.4      tidyselect_1.2.0 utf8_1.2.3      
#> [29] pillar_1.8.1     magrittr_2.0.3   tools_4.3.0
```
