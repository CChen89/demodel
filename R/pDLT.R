DLT.prob <- function(para, drug.name, dose.levels, ref.dose, covariates = NULL, predict = NULL)
{ 
  #########################################
  ####### Estimate DLT Probability ########
  #########################################
  # para: a named data.frame
  # dose.levels: a named list
  # ref.dose: a numeric factor
  # Once we have (posterior) estimates of parameters, we are able to calculate toxic probability for each provisional dose
  #########################################
  
  dose.grid <- expand.grid(dose.levels) 
  Xgrid <- if(is.null(predict)) dose.grid else merge(dose.grid, predict %>% setcolorder(covariates))
  Xgrid %>% data.table() %>% setcolorder(c(drug.name, covariates)) 
  n.drug <- length(dose.levels)
  
  XMat.pre <- data.table(I = 1, Xgrid)
  XMat.pre[, paste0("log.", drug.name, ".dose.ratio") := lapply(1:n.drug, function(i) log(.SD[[i]]/ref.dose[i])), .SDcols = drug.name][, c(drug.name):=NULL,]
  
  alpha.paras <- paste0("alpha", 1:n.drug)
  beta.paras <- paste0("beta", 1:n.drug)
  coef.paras <- if(is.null(predict)) NULL else do.call(c, lapply(paste0("gamma", 1:n.drug), paste0, 1:length(predict)))
  inter.paras <- if(n.drug < 2) NULL else "eta" 
  
  # log transform alphas in MCMC samples ------------------------------------------------------------
  
  XMat <- lapply(1:n.drug, function(i) as.matrix(XMat.pre[, .SD, .SDcols = c("I", paste0("log.", drug.name[i], ".dose.ratio"), colnames(predict))]))
  Para.Mat <- lapply(1:n.drug, function(i) {para[, alpha.paras[i]] <- log(para[, alpha.paras[i]]); as.matrix(para[, c(alpha.paras[i], beta.paras[i], if(is.null(predict)) NULL else do.call(c, lapply(paste0("gamma", i), paste0, 1:length(predict))))])})
  
  pDLT <- lapply(1:n.drug, function(i) 1/(1+exp(-(Para.Mat[[i]]%*%t(XMat[[i]])))))
  
  if(!is.null(inter.paras))
  {
    odds12 <- exp(matrix(para[[inter.paras]], nrow = length(para[[inter.paras]])) %*% matrix(exp(XMat.pre[[paste0("log.", drug.name[1], ".dose.ratio")]]) * exp(XMat.pre[[paste0("log.", drug.name[2], ".dose.ratio")]]), nrow = 1)) * (pDLT[[1]] + pDLT[[2]] - pDLT[[1]] * pDLT[[2]])/((1 - pDLT[[1]]) * (1 - pDLT[[2]]))
    pDLT <- 1/(1 + (odds12)^-1)
  }
  
  return(pDLT = if(n.drug == 1) pDLT[[1]] else pDLT)
}
