#' Input parameters about MCMC sampler
#'
#' @param MCMCpacakge A string indicating which packages will be used. Currently, tested for rjags.
#' @param prior A named list of vectors. First element, 'mean' a vector of mean values of \eqn{log\alpha} and \eqn{\beta}. See details.
#' @param init.list A named list. See \code{\link[rjags]{jags.model}} for more information.
#' @param n.sample A integer or numeric value. The number of MCMC samples retains. See \code{\link[rjags]{jags.model}} for more information.
#' @param n.burnin A integer or numeric value. The number of MCMC samples burned. See \code{\link[rjags]{jags.model}} for more information.
#' @param n.adapt A integer or numeric value. See \code{\link[rjags]{jags.model}} for more information.
#' @param n.thin A integer or numeric value. See \code{\link[rjags]{jags.model}} for more information.
#' @param n.chain A integer or numeric value. See \code{\link[rjags]{jags.model}} for more information.
#' @return A named list
#' @export
#' @examples
#'
#' \dontrun{
#'   Bayes.Info <- BayesInfo(MCMCpackage = "rjags",
#'                           prior = list(mean = list(c(-1.7346, 0)), std = list(c(2, 1)), corr = list(0)),
#'                           init.list = list(list(paras1 = c(-3, 0), .RNG.seed = 1, .RNG.name="base::Wichmann-Hill"),
#'                           list(paras1 = c(-3, 0), .RNG.seed = 2, .RNG.name="base::Wichmann-Hill")),
#'                           n.sample = 10000,
#'                           n.burn = 2000,
#'                           n.adapt = 1000,
#'                           n.chain = 2,
#'                           n.thin = 1)
#' }

BayesInfo <- function(MCMCpackage = "rjags", prior = NULL, init.list = NULL, n.sample = 10000, n.burnin = 2000, n.adapt = 2000, n.thin = 1, n.chain = 2)
{
 return(list(MCMCpackage = MCMCpackage,
             init.list = init.list,
             prior = prior,
             n.sample = n.sample,
             n.burnin = n.burnin,
             n.adapt = n.adapt,
             n.thin = n.thin,
             n.chain = n.chain))
}
