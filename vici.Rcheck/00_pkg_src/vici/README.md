
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vici - Vaccine Induced Cellular Immunogenicity with Bivariate Modeling

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/vici)](https://CRAN.R-project.org/package=vici)
[![Travis build
status](https://travis-ci.org/borishejblum/vici.svg?branch=master)](https://travis-ci.org/borishejblum/vici)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/borishejblum/vici?branch=master&svg=true)](https://ci.appveyor.com/project/borishejblum/vici)
[![Downloads](https://cranlogs.r-pkg.org/badges/vicis?color=blue)](https://www.r-pkg.org/pkg/vici)
<!-- badges: end -->

`vici` encapsulate a shiny app for accurate estimation of vaccine
induced immunogenicity with bivariate linear modeling.

> Lhomme E, Hejblum BP, Lacabaratz C, Wiedemann A, Lelièvre J-D, Levy Y,
> Thiébaut R, Richert L (2019). Analyzing cellular immunogenicity in
> vaccine clinical trials: a new statistical method including
> non-specific responses for accurate estimation of vaccine efficacy.
> *submitted*.

## Installation

You can install the released version of vici from the
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("vici")
```

or get the development version from
[GitHub](https://github.com/borishejblum/vici):

``` r
#install.packages("devtools")
devtools::install_github("borishejblum/vici")
```

Then you can launch the app with:

``` r
vici::run_app()
```

## Deployed VICI

  - Latest **development** version is deployed at
    <https://shiny-vici.apps.math.cnrs.fr/>

  - Latest **stable** version is deployed at
    <http://vici.bph.u-bordeaux.fr/>

– Boris Hejblum
