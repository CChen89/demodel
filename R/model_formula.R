# BLRM model for Dual Agent ---------------------------------------------------------
Model.formula <- function(Combo = FALSE, covariates = NULL)
{
  if(!is.null(covariates))
  {
    cova.formula <- lapply(1:(1+Combo), function(j) paste(do.call(c, lapply(1:length(covariates), function(i) paste0(" + gamma", j, i, " * ", covariates[i], "[i]"))), collapse = ""))
    # coef.dist <- paste(do.call(c, lapply(1:length(covariates), function(i) paste0("gamma1", i, " ~ dnorm(mu.g", i, ", pow(Sigma.g", i, ", -2))", "\n"))), collapse = " ")
    coef.trans <- lapply(1:(1+Combo), function(j) paste(do.call(c, lapply(1:length(covariates), function(i) paste0("gamma", j, i, " <- paras", j,"[", i+2,"]", "\n"))), collapse = " "))
  } else
  {
    cova.formula <- coef.dist <- coef.trans <- NULL
  }

  if(!Combo)
  {
    model.formula<- paste0(" model
  {
   Sigma1[1:P, 1:P] <- inverse(CovMat1[, ])
   paras1[1:P] ~ dmnorm(mu1[], Sigma1[, ])

   alpha1 <- exp(paras1[1])
   beta1 <- exp(paras1[2]) \n ",
                           coef.trans[[1]],
                           " for(i in 1:npat)
   {\n ",
                           paste0("logit(pDLT[i]) <- log(alpha1) + beta1 * log.drug1.dose.ratio[i]", cova.formula[[1]], "\n")
                           ," DLT[i] ~ dbern(pDLT[i])
   }
  }")
  } else
  {
    model.formula <- paste0("model
  {
   Sigma1[1:P,1:P] <- inverse(CovMat1[,])
   paras1[1:P] ~ dmnorm(mu1[], Sigma1[,])

   Sigma2[1:P,1:P] <- inverse(CovMat2[,])
   paras2[1:P] ~ dmnorm(mu2[], Sigma2[,])

   tau <- 1/eta.var
   eta ~ dnorm(eta.mu, tau)

   alpha1 <- exp(paras1[1])
   beta1 <- exp(paras1[2])
   alpha2 <- exp(paras2[1])
   beta2 <- exp(paras2[2])\n ",
                            paste(coef.trans, collapse = " "),
                            " for(i in 1:npat)
   {\n ",
                            paste0("logit(pDLT1[i]) <- log(alpha1) + beta1 * log.drug1.dose.ratio[i]", cova.formula[[1]], "\n"),
                            paste0("logit(pDLT2[i]) <- log(alpha2) + beta2 * log.drug2.dose.ratio[i]", cova.formula[[2]], "\n"),
                            " odds12[i] <- exp(eta * exp(log.drug1.dose.ratio[i]) * exp(log.drug2.dose.ratio[i])) * (pDLT1[i] + pDLT2[i] - pDLT1[i] * pDLT2[i])/((1 - pDLT1[i]) * (1 - pDLT2[i]))
    pDLT[i] <- odds12[i]/(1 + odds12[i])
    DLT[i] ~ dbern(pDLT[i])
   }
  }")
  }

  return(model.formula)
}
