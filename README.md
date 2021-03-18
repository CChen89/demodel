
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demodel

<!-- badges: start -->

<!-- badges: end -->

The goal of demodel is to provide user-friendly functionality for early
phase dose finding trial. This package will be updated for more models.

## Installation

You can install the released version of demodel from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("demodel")
```

## Example

This is a basic example which shows you how to fit early phase trial
data:

``` r
library(demodel)
# design info ----------------------------------------------------------------------------

MB.Info <- MBInfo(dose.levels = list(DSPX = c(80, 120, 160, 200, 240, 280)),
                  ref.dose = 120,
                  bounds = c(0.16, 0.33),
                  ewoc = 0.25,
                  trial.name = "DSP-509",
                  drug.name = "DSP509",
                  drug.unit = "mg")

# Bayes Info ---------------------------------------------------------------------------

Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
                        prior = list(mean = list(c(-1.7346, 0)), std = list(c(2, 1)), corr = list(0)),
                        init.list = list(list(paras1 = c(-3, 0), .RNG.seed = 1, .RNG.name="base::Wichmann-Hill"),
                                         list(paras1 = c(-3, 0), .RNG.seed = 2, .RNG.name="base::Wichmann-Hill")),
                        n.sample = 10000,
                        n.burn = 2000,
                        n.adapt = 1000,
                        n.chain = 2,
                        n.thin = 1)

 demodel_mono <- demodelFit(data = blrm_mono_data,
                            formula = cbind(nDLT, npat) ~ DSPX,
                            method = "blrm",
                            mbdInfo = MB.Info,
                            bayesInfo = Bayes.Info,
                            multiSce = ~ Scenario,
                            control = demodelControl(code.name = "demodel_core.R",
                                                     output.path = NULL, # change to getwd() if require a report
                                                     table.file.name = NULL, # provide a file name if require a report
                                                     fig.file.name = NULL))
```

This results in a summary using tables

``` r
names(demodel_mono$res.summary)
#> [1] "para.Sce.summary"        "pDLT.Sce.summary"       
#> [3] "Interval.Sce.summary"    "Scenario.output.summary"
#> [5] "General.info"            "dose.info"              
#> [7] "MCMC.info"
```

and a summary using figures

``` r
demodel_mono$plot.summary
#> $p.Interval
```

<img src="man/figures/README-figures-1.png" width="100%" />

    #> 
    #> $p.pDLT

<img src="man/figures/README-figures-2.png" width="100%" />

To be continue
