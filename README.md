
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demodel

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/CChen89/demodel.svg?branch=main)](https://travis-ci.com/CChen89/demodel)
[![R-CMD-check](https://github.com/CChen89/demodel/workflows/R-CMD-check/badge.svg)](https://github.com/CChen89/demodel/actions)
[![CircleCI build
status](https://circleci.com/gh/CChen89/demodel.svg?style=svg)](https://circleci.com/gh/CChen89/demodel)
<!-- badges: end -->

The goal of demodel is to provide user-friendly functionality for early
phase dose finding trial. This package will be updated to include more
models.

## Installation

You can install the released version of demodel from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("demodel")
```

## Example 1

This is a basic example which shows you how to fit single-agent trial
data without covariates:

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
                                                     table.file.name = NULL)) # provide a file name if require a report
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

<img src="man/figures/README-SA_figures-1.png" width="100%" /><img src="man/figures/README-SA_figures-2.png" width="100%" />

## Example 2

This is a example which shows you how to fit dual-agent trial data with
covariates:

``` r
library(demodel)
# design info ----------------------------------------------------------------------------

 MB.Info <- MBInfo(dose.levels = list(DSPX1 = c(0.3, 0.6, 1, 1.8, 3), DSPX2 = c(200, 400, 600, 800)), 
                   ref.dose = c(2.4, 400),
                   bounds = c(0.16, 0.33),
                   ewoc = 0.25,
                   trial.name = "DSPX2333",
                   drug.name = c("DSPX1", "DSPX2"),
                   drug.unit = c("mg BID", "mg QD"))

 # Bayes Info ---------------------------------------------------------------------------

 seeds <- 1:4

 Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
                         prior = list(mean = list(c(-1.7346, 0, 0.77, 0.98), c(-2.9444, 0, 0.44, 0.23), 0),
                                      std = list(c(2, 1, 0.5, 1.2), c(2, 1, 1, 1.4), 1.12),
                                      corr = list(diag(1, 4),diag(1, 4))),
                         init.list = list(list(paras1 = c(-3, 0, -0.5, 0.7), paras2 = c(-2, 0, 0.78, 0.23), eta = 0.1, .RNG.seed = seeds[1], .RNG.name = "base::Wichmann-Hill"),
                                          list(paras1 = c(-2, 0, 0.89, -0.98), paras2 = c(-3, 0, 0.32, -0.54), eta = 0, .RNG.seed = seeds[2], .RNG.name = "base::Wichmann-Hill"),
                                          list(paras1 = c(-3, 0.5, 0.65, 0.72), paras2 = c(-3, 0, -0.65, 1.02), eta = 0.1, .RNG.seed = seeds[3], .RNG.name = "base::Wichmann-Hill"),
                                          list(paras1 = c(-2, -0.5, -0.43, 0.67), paras2 = c(-3, 0, -0.98, -1.2), eta = 0, .RNG.seed = seeds[4], .RNG.name = "base::Wichmann-Hill")),
                         n.sample = 10000,
                         n.burn = 2000,
                         n.adapt = 1000,
                         n.chain = 4,
                         n.thin = 1)

 demodel.combo.MS.cov <- demodelFit(data = blrm_combo_cov_data,
                                    formula = DLT ~ DSPX1 + DSPX2 | Azole + Cytarabine,
                                    method = "blrm",
                                    mbdInfo = MB.Info,
                                    bayesInfo = Bayes.Info,
                                    multiSce = ~ Scenario,
                                    control = demodelControl(code.name = "demodel_core.R",
                                                             output.path = NULL, # change to getwd() if require a report,
                                                             table.file.name = NULL)) # provide a file name if require a report
```

This results in a summary using tables

``` r
names(demodel.combo.MS.cov$res.summary)
#> [1] "para.Sce.summary"        "pDLT.Sce.summary"       
#> [3] "Interval.Sce.summary"    "Scenario.output.summary"
#> [5] "General.info"            "dose.info"              
#> [7] "MCMC.info"
```

and a summary using figures

<img src="man/figures/README-Comb_cov_figures-1.png" width="100%" /><img src="man/figures/README-Comb_cov_figures-2.png" width="100%" />

To be continue
