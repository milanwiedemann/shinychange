# shinychange: An Interactive Tutorial on Latent Change Score Modeling in R

The main aim of this interavtive tutorial is to show how different univariate and bivariate latent change score models (lcsm) can be implemented in R using [lavaan](http://lavaan.ugent.be/) syntax (Rosseel, [2012](http://www.jstatsoft.org/v48/i02)).

At the moment it is possible to:
- **Generate lavaan Syntax** for different model specifications and varying time points
- **Simulate Data** to explore the effect of different parameters
- **Fit Models** using example datasets
- Create **Longitudinal Plots** and simplified **Path Diagrams** to visualise data and model specifications 

I started working on this project to better understand how latent change score modeling works.
The underlying functions of this shiny application come from the [lcsm](https://github.com/milanwiedemann/lcsm) R package that I created to make it easier to write lavaan syntax for different models.
The [lcsm](https://github.com/milanwiedemann/lcsm) package combines the strengths of other R packages like [lavaan](http://lavaan.ugent.be/), [broom](https://broom.tidyverse.org), and [semPlot](https://cran.r-project.org/web/packages/semPlot/index.html) by generating lavaan syntax that helps these packages work together.

This is work in progress and feedback is very welcome.
The code of the R package [lcsm](https://github.com/milanwiedemann/lcsm) and [this shiny application](https://github.com/milanwiedemann/shinychange) can be found on GitHub.
Please send me your thoughts on [Twitter](https://twitter.com/milanwiedemann), GitHub, or by [email](mailto:milan.wiedemann@gmail.com).  

### Installation of the R Package lcsm

You can install the development version from
[GitHub](https://github.com/milanwiedemann/lcsm) with:

``` r
# install.packages("devtools")
devtools::install_github("milanwiedemann/lcsm")
```

### Related Work

For details about this methods see for example 
McArdle ([2009](http://www.annualreviews.org/doi/10.1146/annurev.psych.60.110707.163612)),
Grimm et al. ([2012](https://doi.org/10.1080/10705511.2012.659627)), or Usami et al. ([2019](http://dx.doi.org/10.1037/met0000210)).

Examples illustrating how to implement different latent change score models in R can be found for example in:
Ghisletta & McArdle ([2012](https://doi.org/10.1080/10705511.2012.713275)), 
Grimm, Ram & Estabrook ([2017](https://www.guilford.com/books/Growth-Modeling/Grimm-Ram-Estabrook/9781462526062)), Kievit et al., ([2018](https://doi.org/10.1016/j.dcn.2017.11.007)) including a [shiny interface](http://brandmaier.de/shiny/sample-apps/SimLCS_app/), and
Jacobucci et al. ([2019](https://doi.org/10.1080/10705511.2019.1619457)).

Online tutorials from the [Quantitative Developmental Systems Methodology Core](https://quantdev.ssri.psu.edu/) by [Xiao Yang](https://quantdev.ssri.psu.edu/people/xfy5031) and [Miriam Brinberg](https://quantdev.ssri.psu.edu/people/mjb6504) show examples how to specify lavaan syntax for [univariate](https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-16-introduction-latent-change-score-modeling) and [bivariate](https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-17-multivariate-latent-change-score-models) latent change score models described in Grimm, Ram & Estabrook ([2017](https://www.guilford.com/books/Growth-Modeling/Grimm-Ram-Estabrook/9781462526062)).
