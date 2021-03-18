Prior.para <- function(mu, std, corr)
{
  #############################################################
  # mu: a list of length >= 1, means of log alphas and log betas, and other raw coefs
  # std: a list of length >= 1, standard deviations ............
  # corr: a list of length >= 1, correlations .................. 
  #############################################################
  
  Prior.Mean <- lapply(1:length(mu), function(i) mu[[i]])
  names(Prior.Mean) <- c(paste0("mu", 1:length(corr)), if(length(corr) > 1) "eta.mu" else NULL)
  
  Prior.CorMat <- lapply(1:length(corr), function(i) if(is.matrix(corr[[i]])) {corr[[i]]} else {CorMat <- matrix(0, nrow = length(std[[i]]), ncol = length(std[[i]])); CorMat[upper.tri(CorMat, diag = FALSE)] <- corr[[i]]; CorMat <- CorMat + t(CorMat); diag(CorMat)<-1; CorMat}) 
  
  Prior.CovMat <- lapply(1:length(std), 
                         function(i) if(i<=length(corr)) {CovMat.pre <- matrix(1, nrow = length(std[[i]]), ncol = 1) %*% matrix(std[[i]], nrow = 1, ncol = length(std[[i]])); CovMat.pre <- CovMat.pre * t(CovMat.pre); CovMat <- CovMat.pre * Prior.CorMat[[i]]; CovMat} else std[[i]]^2)
  
  names(Prior.CovMat) <- c(paste0("CovMat", 1:length(corr)), if(length(std) >= 3) "eta.var" else NULL)
  
  return(list(mean = Prior.Mean, 
              CovMat = Prior.CovMat))
}
