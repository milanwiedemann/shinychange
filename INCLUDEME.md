# lcsm: An R package and tutorial on latent change score modeling

This shiny application illustrates some functions of the [lcsm](https://github.com/milanwiedemann/lcsm) R package.
The main aim of this application is to show how different univariate and bivariate latent change score models (lcsm) can be implemented in R using [lavaan](http://lavaan.ugent.be/) syntax (Rosseel,
[2012](http://www.jstatsoft.org/v48/i02/)).
This application also provides data simulation tools to explore the effect of different parameters. 

I started working on this project to better understand how latent change score modeling works and to how to use R efficiently to run different models.
This is still work in progress and feedback is very welcome. 
The code of the R package [lcsm](https://github.com/milanwiedemann/lcsm) and this shiny application [shinychange](https://github.com/milanwiedemann/shinychange) can be found on GitHub.
Feel free to contact me on [Twitter](https://twitter.com/milanwiedemann) or send me an [email](mailto:milan.wiedemann@gmail.com).

### Installation of the R package lcsm

You can install the development version from
[GitHub](https://github.com/milanwiedemann/lcsm) with:

``` r
# install.packages("devtools")
devtools::install_github("milanwiedemann/lcsm")
```

### Related work

For details about this methods see for example 
McArdle ([2009](http://www.annualreviews.org/doi/10.1146/annurev.psych.60.110707.163612)),
Grimm et al. ([2012](https://doi.org/10.1080/10705511.2012.659627)).
Examples illustrating how to implement different latent change score models in R can be found for example in:
Ghisletta & McArdle ([2012](https://doi.org/10.1080/10705511.2012.713275)), 
Grimm, Ram & Estabrook ([2017](https://www.guilford.com/books/Growth-Modeling/Grimm-Ram-Estabrook/9781462526062)), and
Kievit et al., ([2018](https://doi.org/10.1016/j.dcn.2017.11.007)).
Online tutorials from the [Quantitative Developmental Systems Methodology Core](https://quantdev.ssri.psu.edu/) by [Xiao Yang](https://quantdev.ssri.psu.edu/people/xfy5031) and [Miriam Brinberg](https://quantdev.ssri.psu.edu/people/mjb6504) show examples how to specify lavaan code for [univariate](https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-16-introduction-latent-change-score-modeling) and [bivariate](https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-17-multivariate-latent-change-score-models) latent change score models described in Grimm, Ram & Estabrook ([2017](https://www.guilford.com/books/Growth-Modeling/Grimm-Ram-Estabrook/9781462526062)).