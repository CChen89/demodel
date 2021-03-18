#' @title Fit dose escalation models
#'
#' @description \code{demodelFit} fit dose escalation model
#'
#' @param data A data.frame or data.table or a string. If data is a string, it should gives the path of trial data. Trial data should be either .xlsx or .csv
#' @param formula A formula. See examples.
#' @param method A string. Currently, only blrm is allowed.
#' @param mbdInfo A named list. Trial information. See example.
#' @param madInfo Currently not available.
#' @param pkpdInfo Currently not available.
#' @param predict a data.frame or data.table. When covariates are included, predict should be provided. Otherwise, every unique combinations of covariates will be considered as predictors.
#' @param multiSce a formula. The column name of Scenario indicator.
#' @param bayesInfo a named list. See \code{\link{BayesInfo}}.
#' @param control a named list. See \code{\link{demodelControl}}.
#' @return This function will generate a excel report including tables and figures
#' @export
#'
#' @import ggplot2
#' @import data.table
#' @importFrom dplyr '%>%'
#' @importFrom stats cor quantile sd
#' @importFrom utils tail
#' @import viridis
#' @import openxlsx
#' @import doParallel
#' @import foreach
#' @import rjags
#' @importFrom Rdpack reprompt
#' @references{
#'   \insertRef{blrm2008}{demodel}
#' }
#' @examples
#' \dontrun{
#' # Single-agent without covariates
#'
#'   # Create multiple scenario data
#'   npat.ms <- c(3, 4, 3, 3, 4, 4)
#'   dose.ms <- c(80, 120, 160, 160, 160, 200)
#'   nDLT.ms <- c(0, 0, 0, 1, 0, 1)
#'   Scenario <- c(1, 2, 3, 3, 4, 4)
#'   data <- data.frame(npat = npat.ms, DSPX = dose.ms, nDLT = nDLT.ms, Scenario = Scenario)
#'
#'
#'   # design info ----------------------------------------------------------------------------
#'
#'   MB.Info <- MBInfo(dose.levels = list(DSPX = c(80, 120, 160, 200, 240, 280)),
#'                     ref.dose = 120,
#'                     bounds = c(0.16, 0.33),
#'                     ewoc = 0.25,
#'                     trial.name = "DSP-509",
#'                     drug.name = "DSP509",
#'                     drug.unit = "mg")
#'
#' # Bayes Info ---------------------------------------------------------------------------
#'
#'   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#'                           prior = list(mean = list(c(-1.7346, 0)), std = list(c(2, 1)), corr = list(0)),
#'                           init.list = list(list(paras1 = c(-3, 0), .RNG.seed = 1, .RNG.name="base::Wichmann-Hill"),
#'                                            list(paras1 = c(-3, 0), .RNG.seed = 2, .RNG.name="base::Wichmann-Hill")),
#'                           n.sample = 10000,
#'                           n.burn = 2000,
#'                           n.adapt = 1000,
#'                           n.chain = 2,
#'                           n.thin = 1)
#'
#'
#'     demodel.MS <- demodelFit(data = data,
#'                               formula = cbind(nDLT, npat) ~ DSPX,
#'                               method = "blrm",
#'                               mbdInfo = MB.Info,
#'                               bayesInfo = Bayes.Info,
#'                               multiSce = ~ Scenario,
#'                               control = demodelControl(code.name = "demodel_core.R",
#'                                                        output.path = getwd(),
#'                                                        table.file.name = "preview.xlsx",
#'                                                        fig.file.name = NULL))
#' }
#'
#' # Combo with covariates
#' \dontrun{
#'
#'   # design info ----------------------------------------------------------------------------
#'
#' MB.Info <- MBInfo(dose.levels = list(DSPX1 = c(0.3, 0.6, 1, 1.8, 3), DSPX2 = c(200, 400, 600, 800)),
#'                   ref.dose = c(2.4, 400),
#'                   bounds = c(0.16, 0.33),
#'                   ewoc = 0.25,
#'                   trial.name = "DSPX2333",
#'                   drug.name = c("DSPX1", "DSPX2"),
#'                   drug.unit = c("mg BID", "mg QD"))
#'
#' # Bayes Info ---------------------------------------------------------------------------
#'
#' seeds <- 1:4
#'
#' Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#'                         prior = list(mean = list(c(-1.7346, 0, 0.77, 0.98), c(-2.9444, 0, 0.44, 0.23), 0),
#'                                      std = list(c(2, 1, 0.5, 1.2), c(2, 1, 1, 1.4), 1.12),
#'                                      corr = list(diag(1, 4),diag(1, 4))),
#'                         init.list = list(list(paras1 = c(-3, 0, -0.5, 0.7), paras2 = c(-2, 0, 0.78, 0.23), eta = 0.1, .RNG.seed = seeds[1], .RNG.name = "base::Wichmann-Hill"),
#'                                          list(paras1 = c(-2, 0, 0.89, -0.98), paras2 = c(-3, 0, 0.32, -0.54), eta = 0, .RNG.seed = seeds[2], .RNG.name = "base::Wichmann-Hill"),
#'                                          list(paras1 = c(-3, 0.5, 0.65, 0.72), paras2 = c(-3, 0, -0.65, 1.02), eta = 0.1, .RNG.seed = seeds[3], .RNG.name = "base::Wichmann-Hill"),
#'                                          list(paras1 = c(-2, -0.5, -0.43, 0.67), paras2 = c(-3, 0, -0.98, -1.2), eta = 0, .RNG.seed = seeds[4], .RNG.name = "base::Wichmann-Hill")),
#'                         n.sample = 10000,
#'                         n.burn = 2000,
#'                         n.adapt = 1000,
#'                         n.chain = 4,
#'                         n.thin = 1)
#'
#' data.path <- "~/BLRM_combo_DEM_Covariates.xlsx"
#'
#' # run BLRM
#' demodel.combo.MS.cov <- demodelFit(data = data.path,
#'                                    formula = DLT ~ DSPX1 + DSPX2 | Azole + Cytarabine,
#'                                    method = "blrm",
#'                                    mbdInfo = MB.Info,
#'                                    bayesInfo = Bayes.Info,
#'                                    multiSce = ~ Scenario,
#'                                    control = demodelControl(code.name = "demodel_core.R",
#'                                                             output.path = getwd(),
#'                                                             table.file.name = "preview.xlsx",
#'                                                             fig.file.name = NULL))
#'
#'
#' }
#'
#'
#'
# Multiple Scenario ---------------------------------------------------------------------------

demodelFit <- function(data,
                       formula = NULL,
                       method = "blrm",
                       mbdInfo = MBInfo(),
                       madInfo = NULL,
                       pkpdInfo = NULL,
                       predict = NULL,
                       multiSce = NULL,
                       bayesInfo = BayesInfo(),
                       control = demodelControl())
{
  ################################################################################################
  ##################################### Input Check: start #######################################
  ################################################################################################

  if(is.null(method)|is.na(method)) stop("Choose appropriate method")
  if(is.null(formula) & !(is.null(mbdInfo))) stop("Model-based designs require valid formula")
  if(is.null(control)|any(is.na(control))) stop("Specify parameters used in parallel computation or MCMC!")

  ################################################################################################
  ##################################### Input Check: end #########################################
  ################################################################################################

  # ------------------------------------------------------------------------------------------------

  # check is there any covariates ---------------------------------------------------------

  if(!is.null(formula))
  {
    Check.name <- formula_check(paste(formula))
    DLT.name <- Check.name$DLT.name
    npat.name <- Check.name$npat.name
    drug.name <- Check.name$drug.name
    covariates <- Check.name$covariates
  }

  Sce.name <- if(is.null(multiSce)) "Scenario" else paste(multiSce)[2]
  if(is.na(Sce.name)) stop("missing column: Scenario Index")

  ################################################################################################
  ##################################### data step: start #########################################
  ################################################################################################

  # Read Scenario.cumu.data -----------------------------------------------------------
  if(is.character(data))
  {
    dec <- tstrsplit(data, split = "[.]")
    if(dec[[length(dec)]] == "xlsx")
    {
      data <- data.table(read.xlsx(data))
    } else
    {
      if(dec[[length(dec)]] == "csv")
      {
        data <- fread(data)
      } else
      {
        stop("Data type is not any of the following: xlsx, csv")
      }
    }
  } else
  {
    data <- data.table(data)
  }

  if(!is.null(npat.name) & is.character(data[[DLT.name]]))
  {
    data <- data[, lapply(.SD, function(x) as.numeric(do.call(c, strsplit(x, split = ",")))), by = Sce.name]
  }
  if(!(Sce.name%in%names(data))) data[[Sce.name]] <- 1

  # check --------------------------------------------------------------------------------------

  if(!is.null(covariates) & !all(covariates%in%colnames(data))) stop("Some specified covariates are not included in data")
  if(!(DLT.name%in%colnames(data))) stop("DLT variable is not included in the trial data: Check spelling")

  ##############################################################################################
  ##################################### data step: end #########################################
  ##############################################################################################

  # --------------------------------------------------------------------------------------------

  # demodel_mf <- match.call(expand.dots = FALSE)
  # demodel_m <- match(c("multiSce", "control"), names(demodel_mf), 0L)
  # demodel_mf <- demodel_mf[c(1L, demodel_m)]
  # demodel_mf[[1L]] <- quote(demodel)

  if(!is.null(predict)) predict <- data.table(predict)

  if(!is.null(covariates) & is.null(predict))
  {
    predict <- unique(data[, .SD, .SDcols = covariates], by = covariates)
  }

  if(is.null(covariates) & !is.null(predict))
  {
    warning("Covariates are not specified: predict will be set to NULL")
    predict <- NULL
  }

  # register cores and conduct parallel computation ---------------------------------------------------------------------------------------
  Sce <- unique(data[[Sce.name]])
  registerDoParallel(control$core)
  demodel.MS <- foreach(i = 1:length(Sce), .combine = Parallel_combine, .packages = c("data.table", "dplyr", "rjags"), .export = c("demodel", "Cohort_to_Pat", "DLT_prob", "formula_check", "Model_formula", "Prior_para", "BLRM_model")) %dopar%
    {
      Sce.data <- data[get(Sce.name) == Sce[i]][, c(Sce.name):=NULL,]

      # res <- eval(demodel_mf, parent.frame())

      res <- demodel(data = Sce.data,
                     formula = formula,
                     method = method,
                     mbdInfo = mbdInfo,
                     madInfo = NULL,
                     pkpdInfo = NULL,
                     predict = predict,
                     bayesInfo = bayesInfo)

      para.Sce.summary <- data.table(Scenario = Sce[i],
                                     res$para.summary)

      NDR.Sce.summary <- data.table(Scenario = Sce[i],
                                    res$NDR.summary)

      pDLT.Sce.summary <- data.table(Scenario = Sce[i],
                                     res$pDLT.summary)

      Interval.Sce.summary <- data.table(Scenario = Sce[i],
                                         res$Interval.prop)

      return(list(pDLT.Sce.summary = pDLT.Sce.summary,
                  Interval.Sce.summary = Interval.Sce.summary,
                  para.Sce.summary = para.Sce.summary,
                  NDR.Sce.summary = NDR.Sce.summary))
    }
  stopImplicitCluster()

  trans.results <- list(pDLT.Sce.summary = demodel.MS$pDLT.Sce.summary,
                        Interval.Sce.summary = demodel.MS$Interval.Sce.summary,
                        para.Sce.summary = demodel.MS$para.Sce.summary,
                        NDR.Sce.summary = demodel.MS$NDR.Sce.summary)

  trans.plot.data <- trans.plot(trans.results = trans.results, formula = formula, dose.levels = mbdInfo$dose.levels, ewoc = mbdInfo$ewoc, int.cut = mbdInfo$bounds, multiSce.var = Sce.name)

  res.summary <- table.summary(trans.results = trans.results, data = data, formula = formula, trialInfo = mbdInfo, bayesInfo = bayesInfo, ewoc = mbdInfo$ewoc, int.cut = mbdInfo$bounds, multiSce.var = Sce.name)

  plot.summary <- plot.summary(trans.plot.data = trans.plot.data, formula = formula, predict = predict, ewoc = mbdInfo$ewoc, int.cut = mbdInfo$bounds, dose.unit = mbdInfo$drug.unit, multiSce.var = Sce.name)

  if(!is.null(control$table.path))
  {
    file.prefix <- "tempFigFile"
    output.excel <- table.summary.output(res.summary = res.summary,
                                                   formula = formula,
                                                   data = data,
                                                   multiSce.var = Sce.name,
                                                   figs = plot.summary,
                                                   file.prefix = file.prefix,
                                                   code.file.name = control$code.name,
                                                   width = control$fig.width,
                                                   height = control$fig.height)

    file.name <- if(is.null(control$table.path)) paste(control$date, MBInfo$trial.name, "output.xlsx", sep = "_") else control$table.path

    saveWorkbook(wb = output.excel, file = file.name, overwrite = TRUE)

    file.remove(paste0(file.prefix, 1, ".png"))
    file.remove(paste0(file.prefix, 2, ".png"))
  }

  return(list(res.summary = res.summary,
              plot.summary = plot.summary))
}

# combine results when using parallel computing

Parallel_combine <- function(list1, list2)
{
  pDLT.Sce.summary <- rbind(list1$pDLT.Sce.summary, list2$pDLT.Sce.summary)
  Interval.Sce.summary <- rbind(list1$Interval.Sce.summary, list2$Interval.Sce.summary)
  para.Sce.summary <- rbind(list1$para.Sce.summary, list2$para.Sce.summary)
  NDR.Sce.summary <- rbind(list1$NDR.Sce.summary, list2$NDR.Sce.summary)
  return(list(pDLT.Sce.summary = pDLT.Sce.summary,
              Interval.Sce.summary = Interval.Sce.summary,
              para.Sce.summary = para.Sce.summary,
              NDR.Sce.summary = NDR.Sce.summary))
}

# # Mono: No covariate ------------------------------------------------------------------------------------
# if(F)
# {
#   # Drug name and time -------------------------------------------------------------------
#
#   file.name.BLRM <- paste(Drug.name, format(Sys.time(), '%Y%m%d'), "BLRM", sep = "_")
#   code.file.name <- "demodel_core.R"
#
#   # design info ----------------------------------------------------------------------------
#
#   MB.Info <- MBInfo(dose.levels = list(DSPX = c(80, 120, 160, 200, 240, 280)),
#                     ref.dose = 120,
#                     bounds = c(0.16, 0.33),
#                     ewoc = 0.25,
#                     trial.name = "DSP-509",
#                     drug.name = "DSP509",
#                     drug.unit = "mg")
#
#   # Bayes Info ---------------------------------------------------------------------------
#
#   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#                           prior = list(mean = list(c(-1.7346, 0)), std = list(c(2, 1)), corr = list(0)),
#                           init.list = list(list(paras1 = c(-3, 0), .RNG.seed = 1, .RNG.name="base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0), .RNG.seed = 2, .RNG.name="base::Wichmann-Hill")),
#                           n.sample = 10000,
#                           n.burn = 2000,
#                           n.adapt = 1000,
#                           n.chain = 2,
#                           n.thin = 1)
#
#   # Solution 1: create multiple scenario data using data.frame
#   if(F)
#   {
#     # Create multiple scenario data
#     npat.ms <- c(3, 4, 3, 3, 4, 4)
#     dose.ms <- c(80, 120, 160, 160, 160, 200)
#     nDLT.ms <- c(0, 0, 0, 1, 0, 1)
#     Scenario <- c(1, 2, 3, 3, 4, 4)
#     data <- data.frame(npat = npat.ms, DSPX = dose.ms, nDLT = nDLT.ms, Scenario = Scenario)
#
#     demodel.MS <- demodelFit(data = data,
#                              formula = cbind(nDLT, npat) ~ DSPX,
#                              method = "blrm",
#                              mbdInfo = MB.Info,
#                              bayesInfo = Bayes.Info,
#                              multiSce = ~ Scenario,
#                              control = demodelControl(code.name = "demodel_core.R",
#                                                       output.path = getwd(),
#                                                       table.file.name = "preview.xlsx",
#                                                       fig.file.name = NULL))
#   }
#
#   # Solution 2: read excel
#   if(F)
#   {
#     # data.path
#     data.path <- "D:/Boston Biomedical/Package/BLRM_mono_HDA.xlsx"
#
#     BLRM.MS <- demodelFit(data = data.path,
#                           formula = cbind(nDLT, npat) ~ DSPX,
#                           method = "blrm",
#                           mbdInfo = MB.Info,
#                           bayesInfo = Bayes.Info,
#                           multiSce = ~ Scenario,
#                           control = demodelControl(code.name = "demodel_core.R",
#                                                    output.path = getwd(),
#                                                    table.file.name = "preview.xlsx",
#                                                    fig.file.name = NULL))
#   }
# }
#
# # Mono: Covariates ----------------------------------------------------------------------------
#
# if(F)
# {
#   # design info ----------------------------------------------------------------------------
#
#   MB.Info <- MBInfo(dose.levels = list(DSPX = c(80, 120, 160, 200, 240, 280)),
#                     ref.dose = 120,
#                     bounds = c(0.16, 0.33),
#                     ewoc = 0.25,
#                     trial.name = "DSP666",
#                     drug.name = "DSPX",
#                     drug.unit = "mg QD")
#
#   # Bayes Info ---------------------------------------------------------------------------
#
#   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#                           prior = list(mean = list(c(-1.7346, 0, 0.6)), std = list(c(2, 1, 0.975)), corr = list(diag(1, 3))),
#                           init.list = list(list(paras1 = c(-3, 0, -0.5), .RNG.seed = 1, .RNG.name="base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0, 0.5), .RNG.seed = 2, .RNG.name="base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0, -1), .RNG.seed = 3, .RNG.name="base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0, 1), .RNG.seed = 4, .RNG.name="base::Wichmann-Hill")),
#                           n.sample = 10000,
#                           n.burn = 2000,
#                           n.adapt = 1000,
#                           n.chain = 4,
#                           n.thin = 1)
#
#   # data.path
#   data.path <- "D:/Boston Biomedical/Package/BLRM_mono_DEM_Covariates.xlsx"
#
#   demodel.MS.Cova <- demodelFit(data = data.path,
#                                 formula = DLT ~ DSPX | Azole,
#                                 method = "blrm",
#                                 mbdInfo = MB.Info,
#                                 bayesInfo = Bayes.Info,
#                                 multiSce = ~ Scenario,
#                                 control = demodelControl(code.name = "demodel_core.R",
#                                                          output.path = getwd(),
#                                                          table.file.name = "preview.xlsx",
#                                                          fig.file.name = NULL))
# }
#
# # Test for Combo
# if(F)
# {
#   # design info ----------------------------------------------------------------------------
#
#   MB.Info <- MBInfo(dose.levels = list(DSPX1 = c(0.3, 0.6, 1, 1.8, 3), DSPX2 = c(200, 400, 600, 800)),
#                     ref.dose = c(2.4, 400),
#                     bounds = c(0.16, 0.33),
#                     ewoc = 0.25,
#                     trial.name = "DSPX507",
#                     drug.name = c("DSPX1", "DSPX2"),
#                     drug.unit = c("mg BID", "mg QD"))
#
#   # Bayes Info ---------------------------------------------------------------------------
#
#   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#                           prior = list(mean = list(c(-1.7346, 0), c(-2.9444, 0), 0),
#                                        std = list(c(2, 1), c(2, 1), 1.12),
#                                        corr = list(0,0)),
#                           init.list = list(list(paras1 = c(-3, 0), paras2 = c(-2, 0), eta = 0.1, .RNG.seed = 1, .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-2, 0), paras2 = c(-3, 0), eta = 0, .RNG.seed = 2, .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0.5), paras2 = c(-3, 0), eta = 0.1, .RNG.seed = 3, .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-2, -0.5), paras2 = c(-3, 0), eta = 0, .RNG.seed = 4, .RNG.name = "base::Wichmann-Hill")),
#                           n.sample = 10000,
#                           n.burn = 2000,
#                           n.adapt = 1000,
#                           n.chain = 4,
#                           n.thin = 1)
#
#   # Single Scenario
#   if(F)
#   {
#     data <- data.frame(Scenario = 1, npat = 3, nDLT = 0, DSPX1 = 0.3, DSPX2 = 200)
#     demodel.Combo <- demodelFit(data = data,
#                                 formula = cbind(nDLT, npat) ~ DSPX1 + DSPX2,
#                                 method = "blrm",
#                                 mbdInfo = MB.Info,
#                                 bayesInfo = Bayes.Info,
#                                 multiSce = ~ Scenario,
#                                 control = demodelControl(code.name = "demodel_core.R",
#                                                          output.path = getwd(),
#                                                          table.file.name = "preview.xlsx",
#                                                          fig.file.name = NULL))
#   }
#   # multiple scenario
#   # Solution 1: use scenario.cumu.data method
#   if(F)
#   {
#     # create scenario.cumu.data first
#     npat.combo.ms <- c(1, 2, 3, 3, 4, 5, 1, 2, 3)
#     nDLT.combo.ms <- c(0, 0, 1, 0, 1, 2, 0, 0, 1)
#     drug1.dose <- c(0.3, 0.6, 1, 0.6, 1, 1.8, 0.6, 1, 3)
#     drug2.dose <- c(200, 200, 300, 200, 350, 350, 200, 300, 300)
#     Scenario.combo.ms <- c(1, 2, 2, 3, 3, 3, 4, 4, 4)
#
#     data <- data.frame(Scenario = Scenario.combo.ms,
#                        DSPX1 = drug1.dose,
#                        DSPX2 = drug2.dose,
#                        npat = npat.combo.ms,
#                        nDLT = nDLT.combo.ms)
#
#     demodel.combo.MS <- demodelFit(data = data,
#                                    formula = cbind(nDLT, npat) ~ DSPX1 + DSPX2,
#                                    method = "blrm",
#                                    mbdInfo = MB.Info,
#                                    bayesInfo = Bayes.Info,
#                                    multiSce = ~ Scenario,
#                                    control = demodelControl(code.name = "demodel_core.R",
#                                                             output.path = getwd(),
#                                                             table.file.name = "preview.xlsx",
#                                                             fig.file.name = NULL))
#   }
#
#   # Solution 2: use data path
#   if(F)
#   {
#     data.path <- "D:/Boston Biomedical/Package/BLRM_combo_HDA.xlsx"
#
#     # run BLRM
#     demodel.combo.MS <- demodelFit(data = data.path,
#                                    formula = cbind(nDLT, npat) ~ DSPX1 + DSPX2,
#                                    method = "blrm",
#                                    mbdInfo = MB.Info,
#                                    bayesInfo = Bayes.Info,
#                                    multiSce = ~ Scenario,
#                                    control = demodelControl(code.name = "demodel_core.R",
#                                                             output.path = getwd(),
#                                                             table.file.name = "preview.xlsx",
#                                                             fig.file.name = NULL))
#   }
# }
#
# # Combo with covariates ----------------------------------------------------------------------
#
# if(F)
# {
#   # design info ----------------------------------------------------------------------------
#
#   MB.Info <- MBInfo(dose.levels = list(DSPX1 = c(0.3, 0.6, 1, 1.8, 3), DSPX2 = c(200, 400, 600, 800)),
#                     ref.dose = c(2.4, 400),
#                     bounds = c(0.16, 0.33),
#                     ewoc = 0.25,
#                     trial.name = "DSPX2333",
#                     drug.name = c("DSPX1", "DSPX2"),
#                     drug.unit = c("mg BID", "mg QD"))
#
#   # Bayes Info ---------------------------------------------------------------------------
#
#   seeds <- 1:4
#
#   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#                           prior = list(mean = list(c(-1.7346, 0, 0.77, 0.98), c(-2.9444, 0, 0.44, 0.23), 0),
#                                        std = list(c(2, 1, 0.5, 1.2), c(2, 1, 1, 1.4), 1.12),
#                                        corr = list(diag(1, 4),diag(1, 4))),
#                           init.list = list(list(paras1 = c(-3, 0, -0.5, 0.7), paras2 = c(-2, 0, 0.78, 0.23), eta = 0.1, .RNG.seed = seeds[1], .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-2, 0, 0.89, -0.98), paras2 = c(-3, 0, 0.32, -0.54), eta = 0, .RNG.seed = seeds[2], .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-3, 0.5, 0.65, 0.72), paras2 = c(-3, 0, -0.65, 1.02), eta = 0.1, .RNG.seed = seeds[3], .RNG.name = "base::Wichmann-Hill"),
#                                            list(paras1 = c(-2, -0.5, -0.43, 0.67), paras2 = c(-3, 0, -0.98, -1.2), eta = 0, .RNG.seed = seeds[4], .RNG.name = "base::Wichmann-Hill")),
#                           n.sample = 10000,
#                           n.burn = 2000,
#                           n.adapt = 1000,
#                           n.chain = 4,
#                           n.thin = 1)
#
#   data.path <- "D:/Boston Biomedical/Package/BLRM_combo_DEM_Covariates.xlsx"
#
#   # run BLRM
#   demodel.combo.MS.cov <- demodelFit(data = data.path,
#                                      formula = DLT ~ DSPX1 + DSPX2 | Azole + Cytarabine,
#                                      method = "blrm",
#                                      mbdInfo = MB.Info,
#                                      bayesInfo = Bayes.Info,
#                                      multiSce = ~ Scenario,
#                                      control = demodelControl(code.name = "demodel_core.R",
#                                                               output.path = getwd(),
#                                                               table.file.name = "preview.xlsx",
#                                                               fig.file.name = NULL))
# }
