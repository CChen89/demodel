# BLRM.model() is the core function. It uses MCMC methods (e.g Gibbs sampler) either runjags or rjags

BLRM_model <- function(formula = NULL,
                       data,
                       predict = NULL,
                       dose.levels,
                       ref.dose,
                       int.cut,
                       ewoc,
                       n.adapt = 1000,
                       n.sample = 5000,
                       n.chains = 2,
                       n.thin = 1,
                       n.burnin = 2000,
                       init.list = init.list,
                       package = "rjags")
{
  ############################################################################
  ############# BLRM for single/Dual agent: model specification ##############
  ############################################################################
  # formula: a string, should be specified when there are covariates
  # data: a list including all variables used in MCMC
  # predict: a data.table, values of predictors if there are covariates. Colnames should be same as covariates
  # dose.levels: a named list, length of 1 for Mono, 2 for Combo
  # ...: arguments passed to run.jags/rjags
  #######################################################################

  # n.burnin and n.adapt arguments will automatically drop the first burnin + adapt samples
  # total number of generated samples is actually (n.burnin + n.adapt + n.sample + (n.burnin + n.adapt + n.sample-n.thin) * (n.thin - 1)) * n.chains
  # e.g. n.burnin = 4000, n.adapt = 1000, n.sample = 10000, n.chains = 2 and n.thin = 1 will generate 30000 samples but drop the first 5000 for each chain

  Check.name <- formula_check(paste(formula))
  DLT.name <- Check.name$DLT.name
  npat.name <- Check.name$npat.name
  drug.name <- Check.name$drug.name
  covariates <- Check.name$covariates
  n.drug <- length(dose.levels)

  if(any(!(names(dose.levels)%in%drug.name))) stop("names of dose levels should exactly match those in trial data")

  alpha.paras <- paste0("alpha", 1:length(dose.levels))
  beta.paras <- paste0("beta", 1:length(dose.levels))
  coef.paras <- if(is.null(covariates)) NULL else do.call(c, lapply(paste0("gamma", 1:n.drug), paste0, 1:length(covariates)))
  inter.paras <- if(length(dose.levels) < 2) NULL else "eta" # can be used for Mono and Combo but need to be edited if k > 3 drugs were applied

  model.formula <- Model_formula(Combo = (length(dose.levels) == 2), covariates = covariates)

  if(package == "rjags")
  {
    # solution 2: rjags package: Advantage (conventional, more useful documents can be found than runjags) 05/04/2020
    BLRM.JAGS.MCMC <- rjags::jags.model(textConnection(model.formula),
                                        data = data,
                                        quiet = TRUE,
                                        n.chains = n.chains,
                                        inits = init.list,
                                        n.adapt = n.adapt)
    # Strategy 1: update n.burnin samples + coda.samples, see Doing Bayesian data analysis, page 216
    # Note: this strategy is considered to potentially generate conservative next dose in DSP509
    update(BLRM.JAGS.MCMC, n.iter = n.burnin*n.thin, progress.bar="none")
    BLRM.JAGS <- rjags::coda.samples(BLRM.JAGS.MCMC,
                                     variable.names = c(alpha.paras, beta.paras, coef.paras, inter.paras),
                                     n.iter = ceiling(n.sample*n.thin/n.chains),
                                     thin = n.thin,
                                     progress.bar = "none")
    BLRM.mcmc <- do.call(rbind, lapply(BLRM.JAGS, function(x) data.frame(x)))

    # strategy 2: update (n.burnin + n.sample)/n.chain samples + coda.samples, a modification of BLRM 1.0 ----------------
    # update(BLRM.JAGS.MCMC, n.iter = (n.burnin + n.sample)/n.chains, progress.bar="none")
    # BLRM.JAGS <- rjags::coda.samples(BLRM.JAGS.MCMC,
    #                                  variable.names = c(alpha.paras, beta.paras, coef.paras, inter.paras),
    #                                  n.iter = ceiling((n.burnin + n.sample)*n.thin/n.chains),
    #                                  thin = n.thin,
    #                                  progress.bar = "none")
    # BLRM.mcmc <- do.call(rbind, lapply(BLRM.JAGS, function(x) data.frame(x)[-c(1:((n.burnin*n.thin)/n.chains)),]))
    #
    # strategy 3: update (n.burnin + n.sample)/n.chain samples + jags.samples, based on BLRM 1.0 -----------------------------------------
    # update(BLRM.JAGS.MCMC, n.iter = (n.burnin + n.sample)/n.chains, progress.bar="none")
    # BLRM.JAGS <- rjags::jags.samples(BLRM.JAGS.MCMC,
    #                                  variable.names = c(alpha.paras, beta.paras, coef.paras, inter.paras),
    #                                  n.iter = ceiling((n.burnin + n.sample)*n.thin/n.chains),
    #                                  thin = n.thin,
    #                                  progress.bar = "none")
    # BLRM.mcmc <- do.call(data.frame, lapply(BLRM.JAGS, function(x) c(x[1,,][-c(1:((n.burnin*n.thin)/n.chains)),])))
  }

  if(package == "Rstan")
  {
    # Soluation 3: Rstan package: advatange (theoritically faster than rjags: to be confirmed)
    stop("Rstan Version is still in development")
  }

  # summarize posterior estimates ------------------------------------------------------------------------------------------

  posterior.mean <- apply(data.frame(log(BLRM.mcmc[, c(alpha.paras, beta.paras)]), BLRM.mcmc[, coef.paras], eta = BLRM.mcmc[, inter.paras]), 2, mean) # calculate empirical estimate of posterior mean
  posterior.sd <- apply(data.frame(log(BLRM.mcmc[, c(alpha.paras, beta.paras)]), BLRM.mcmc[, coef.paras], eta = BLRM.mcmc[, inter.paras]), 2, sd)     # calculate empirical estimate of posterior standard deviation
  posterior.cor <- data.frame(lapply(1:length(dose.levels), function(i) cor(log(BLRM.mcmc[, alpha.paras[i]]), log(BLRM.mcmc[, beta.paras[i]])))) # calculate empirical estimate of correlation coefficients
  names(posterior.mean) <- c(paste("Mean", "log", c(alpha.paras, beta.paras), sep = "."), if(is.null(covariates)) NULL else paste0("Mean.", coef.paras), if(is.null(inter.paras)) inter.paras else paste0("Mean.", inter.paras))
  names(posterior.sd) <- c(paste("Std", "log", c(alpha.paras, beta.paras), sep = "."), if(is.null(covariates)) NULL else paste0("Std.", coef.paras), if(is.null(inter.paras)) inter.paras else paste0("Std.", inter.paras))
  names(posterior.cor) <- paste0("corr", 1:length(posterior.cor))
  para.summary <- data.frame(c(posterior.mean, posterior.sd, posterior.cor))

  # calculate the estimates of DLT probability -----------------------------------------------------------------------------

  dose.grid <- expand.grid(dose.levels)
  Xgrid <- if(is.null(predict)) dose.grid else merge(dose.grid, predict)
  Xgrid %>% data.table() %>% setcolorder(c(drug.name, covariates))

  posterior.pDLT <- DLT_prob(para = BLRM.mcmc, drug.name = drug.name, dose.levels = dose.levels, ref.dose = ref.dose, covariates = covariates, predict = predict)

  # summarize the MCMC estimates of probability of DLT ---------------------------------------------------------------------------------------------------

  posterior.pDLT.summary <- data.table(posterior.pDLT)[,.(Mean = do.call(c, lapply(.SD, mean)),
                                                          Std = do.call(c, lapply(.SD, sd)),
                                                          '2.5%' = do.call(c, lapply(.SD, quantile, probs = 0.025)),
                                                          '50%' = do.call(c, lapply(.SD, quantile, probs = 0.5)),
                                                          '97.5%' = do.call(c,lapply(.SD, quantile, probs = 0.975))),]
  posterior.pDLT.summary[, colnames(Xgrid) := Xgrid, ]

  # interval probability ---------------------------------------------------------------------------------------------------------------------------------
  Interval.category <- names(table(cut(c(0, sort(int.cut, decreasing = FALSE), 1), breaks = c(0, sort(int.cut, decreasing = FALSE), 1), include.lowest = TRUE, right = FALSE)))

  Interval.prop <- apply(posterior.pDLT, 2, function(x) table(cut(x, breaks = c(0, int.cut, 1), include.lowest = TRUE, right = FALSE)))/nrow(posterior.pDLT)
  Interval.prop <- data.table(Xgrid, t(Interval.prop))
  Interval.prop[, EWOC := lapply(.SD, ">=", ewoc), .SDcols = tail(Interval.category, 1)]

  # Select the next dose with the highest probability of targeted-toxicity among safe doses --------------------------------------------------------------
  # FALSE: safe(< ewoc), TRUE: non-safe(> ewoc)
  # For simplicity, currently tested with the case of length(ewoc) = 1
  # Note: if there is no NDR, NDR summary will return NA

  NDR.summary <- data.table::merge.data.table(posterior.pDLT.summary,
                                              Interval.prop[EWOC == 0][, .SD[which.max(get(Interval.category[2]))], by = covariates],
                                              by = colnames(Xgrid))

  NDR.summary[, colnames(NDR.summary) := lapply(.SD, function(x) if(is.numeric(x)) round(x, 4)), .SDcols = colnames(NDR.summary)]

  # If there are covariates, collect results for all combinations of combinations
  if(!is.null(predict))
  {
    NDR.summary <- data.table::merge.data.table(data.table(predict), NDR.summary, by = covariates, all.x = TRUE)
  }

  NDR.summary %>% setnames(old = drug.name, new = paste0(drug.name, ".NDR"))

  return(list(para.summary = para.summary,
              pDLT.summary = posterior.pDLT.summary,
              Interval.prop = Interval.prop,
              NDR.summary = NDR.summary,
              MCMCsamples = BLRM.mcmc))
}
