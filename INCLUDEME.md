# lcsm: An R package and tutorial on latent change score modeling

This shiny application illustrates some functions of the lcsm R package.
The main aim of this application is to show how different univariate and bivariate latent change score models (lcsm) can be implemented in R using [lavaan](http://lavaan.ugent.be/) syntax (Rosseel,
[2012](http://www.jstatsoft.org/v48/i02/)).
This application also provides data simulation tools to explore the effect of different parameters. 

For details about this method see for example McArdle
([2009](http://www.annualreviews.org/doi/10.1146/annurev.psych.60.110707.163612)),
Ghisletta ([2012](https://doi.org/10.1080/10705511.2012.713275)), Grimm
et al. ([2012](https://doi.org/10.1080/10705511.2012.659627)), and
Grimm, Ram & Estabrook
([2017](https://www.guilford.com/books/Growth-Modeling/Grimm-Ram-Estabrook/9781462526062)).

### Installation of the R package `lcsm`

You can install the development version from
[GitHub](https://github.com/milanwiedemann/lcsm) with:

``` r
# install.packages("devtools")
devtools::install_github("milanwiedemann/lcsm")
```
