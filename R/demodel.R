demodel <- function(data,
                    formula = NULL,
                    method = "blrm",
                    mbdInfo = MBInfo(),
                    madInfo = NULL,
                    pkpdInfo = NULL,
                    predict = NULL,
                    bayesInfo = BayesInfo())
{
 ################################################################################################
 ##################################### Input Check: start #######################################
 ################################################################################################

 if(is.null(method)|is.na(method)) stop("Choose appropriate method")
 if(is.null(formula) & !(is.null(mbdInfo))) stop("Model-based designs require valid formula")

 ################################################################################################
 ##################################### Input Check: end #########################################
 ################################################################################################

 # ------------------------------------------------------------------------------------------------

 ################################################################################################
 ##################################### data step: start #########################################
 ################################################################################################

 # check is there any covariates ---------------------------------------------------------

  Check.name <- formula_check(paste(formula))
  DLT.name <- Check.name$DLT.name
  npat.name <- Check.name$npat.name
  drug.name <- Check.name$drug.name
  covariates <- Check.name$covariates

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

  # if(!is.null(npat.name) & is.character(data[[DLT.name]]))
  # {
  #  data <- data[, lapply(.SD, function(x) as.numeric(do.call(c, strsplit(x, split = ",")))), by = Sce.name]
  # }

 ##############################################################################################
 ##################################### data step: end #########################################
 ##############################################################################################

 # --------------------------------------------------------------------------------------------

 ################################################################################################
 ################################## Model-based Design: start ###################################
 ################################################################################################

 if(method%in%c("crm","blrm"))
 {
   # extract dose and general info ---------------------------------------------------------

   dose.levels <- mbdInfo$dose.levels
   ref.dose <- mbdInfo$ref.dose
   int.cut <- mbdInfo$bounds
   ewoc <- mbdInfo$ewoc
   Drug.name <- mbdInfo$drug.name
   Drug.unit <- mbdInfo$drug.unit

   n.drug <- length(dose.levels)

   # extract MCMC parameters
   Prior <- Prior_para(mu = bayesInfo$prior$mean,
                                std = bayesInfo$prior$std,
                                corr = bayesInfo$prior$corr)

   n.sample <- bayesInfo$n.sample
   n.burnin <- bayesInfo$n.burn
   n.adapt <- bayesInfo$n.adapt
   n.thin <- bayesInfo$n.thin
   n.chains <- bayesInfo$n.chain
   init.list <- bayesInfo$init.list

   MCMCpackage <- bayesInfo$MCMCpackage

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

   # BLRM ----------------------------------------------------------------------------------
   if(method == "blrm")
   {
    if(is.null(mbdInfo)) stop("a lack of necessary information on design options!")

    # convert cohort-wise data to patient-wise data -----------------------------------------

    if(is.null(npat.name))
    {
     data.pw <- data[, paste0("log.", drug.name, ".dose.ratio") := lapply(1:n.drug, function(i) log(.SD[[i]]/ref.dose[i])), .SDcols = drug.name][, c(drug.name) := NULL,]
    } else
    {
     data.pw <- Cohort_to_Pat(cumu.data = data,
                              ref.dose = ref.dose,
                              variable.names = Check.name)
    }

    if(n.drug <= 1)
    {
     data.Bayes <- list(npat = nrow(data.pw),
                        log.drug1.dose.ratio = data.pw[[paste0("log.", drug.name, ".dose.ratio")]],
                        DLT = data.pw[[DLT.name]],
                        mu1 = Prior$mean$mu1,
                        CovMat1 = Prior$CovMat$CovMat1,
                        P = 2 + length(covariates))
    } else
    {
     data.Bayes <- list(npat = nrow(data.pw),
                        log.drug1.dose.ratio = data.pw[[paste0("log.", drug.name[1], ".dose.ratio")]],
                        log.drug2.dose.ratio = data.pw[[paste0("log.", drug.name[2], ".dose.ratio")]],
                        DLT = data.pw[[DLT.name]],
                        mu1 = Prior$mean$mu1,
                        mu2 = Prior$mean$mu2,
                        eta.mu = Prior$mean$eta.mu,
                        CovMat1 = Prior$CovMat$CovMat1,
                        CovMat2 = Prior$CovMat$CovMat2,
                        eta.var = Prior$CovMat$eta.var,
                        P = 2 + length(covariates))
    }

   if(!is.null(covariates))
   {
     for(i in 1:length(covariates))
     {
       data.Bayes[[covariates[i]]] <- data[[covariates[i]]]
     }
   }

    res <- BLRM_model(formula = formula,
                                data = data.Bayes,
                                predict = predict,
                                dose.levels = dose.levels,
                                ref.dose = ref.dose,
                                int.cut = int.cut,
                                ewoc = ewoc,
                                init.list = init.list,
                                n.burnin = n.burnin,
                                n.sample = n.sample,
                                n.chains = n.chains,
                                n.adapt = n.adapt,
                                n.thin = n.thin,
                                package = MCMCpackage)
   }

 }

 ##############################################################################################
 ################################## Model-based Design: end ###################################
 ##############################################################################################

 return(res)
}
