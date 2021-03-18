library(demodel)

testthat::test_that("formulac output: single-agent", {
  expect_identical(formula.check(paste(DLT ~ DSPX)), list(DLT.name = "DLT", npat.name = NULL, drug.name = "DSPX", covariates = NULL))
  expect_identical(formula.check(paste(cbind(nDLT, npat) ~ DSPX)), list(DLT.name = "nDLT", npat.name = npat, drug.name = "DSPX", covariates = NULL))
  expect_identical(formula.check(paste(DLT ~ DSPX | Azole)), list(DLT.name = "DLT", npat.name = NULL, drug.name = "DSPX", covariates = "Azole"))
})

testthat::test_that("formulac output: Duale-agent", {
  expect_identical(formula.check(paste(DLT ~ DSPX1 + DSPX2)), list(DLT.name = "DLT", npat.name = NULL, drug.name = c("DSPX1", "DSPX2"), covariates = NULL))
  expect_identical(formula.check(paste(cbind(nDLT, npat) ~ DSPX1 + DSPX2)), list(DLT.name = "nDLT", npat.name = npat, drug.name = c("DSPX1", "DSPX2"), covariates = NULL))
  expect_identical(formula.check(paste(DLT ~ DSPX1 + DSPX2 | Azole)), list(DLT.name = "DLT", npat.name = NULL, drug.name = c("DSPX1", "DSPX2"), covariates = "Azole"))
})
