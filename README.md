
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

Version 0.0.3 changed `full_foa` to `opportunity_number` throughout, for
consistency with the NIH Reporter interface.

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

- [Get started](articles/grantpubcite.html) for an introduction to the
  package.
- [Case study: IMAT](articles/case_study_imat.html) looks at the NIH
  Innovative Molecular Analysis Technologies Program aimed at the
  development and integration of novel and emerging technologies in the
  support of cancer research, diagnosis, and treatment.
- [Case study: IOTN](articles/case_study_iotn.html) looks at the NIH NCI
  Immuno-Oncology Translation Network portfolio of grants; the IOTN
  servers as an interesting ‘wet-lab’ comparison for ITCR.
- [Case study: ITCR](articles/case_study_itcr.html) for an illustration
  of package use analysing grants in the NIH Information Technology in
  Cancer Research portfolio of grants. The ITCR is particularly
  interesting because of it’s focus on ‘dry-lab’ technology / software,
  multi-year duration, and sequence of grants representing a progression
  from small grants emphasizing innovation to large grants to sustain
  essential resources.
- [Case study: specific
  projects](articles/case_study_project_nums_quick.html) provides a
  quick explorartion of publications and citations arising from two
  related grants.
- [Implementation notes](articles/implementation_notes.html) provides
  brief notes on implementation, to highligth the great packages &
  people contributing indirectly to the package.

Please note that I am not experienced in this sort of analysis, so any
conclusions should be viewed with great skepticism.
