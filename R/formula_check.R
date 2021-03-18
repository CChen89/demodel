#' @title create formula used in Bayesian modeling
#'
#' @param formula A string. See example.
#' @return A named list with 4 elements
#' @export
#'
#' @import data.table
#' @examples
#'
#' \dontrun{
#'
#' formula.check(paste(DLT ~ DSPX | Azole))
#'
#' }

formula.check <- function(formula)
{
  variable.name <- tstrsplit(gsub(" ", "", mgsub::mgsub(formula[2], c("cbind","\\(", "\\)"), rep("", 3))), split = ",")
  DLT.name <- variable.name[[1]]
  npat.name <- if(length(variable.name)>1) variable.name[[2]] else NULL
  drug.name <- gsub(" ", "", do.call(c, tstrsplit(tstrsplit(formula[3], split = "[|]")[[1]], split = "[+]")))
  covariates <- if(length(tstrsplit(formula[3], split = "[|]")) == 2) gsub(" ", "", do.call(c, tstrsplit(tstrsplit(formula[3], split = "[|]")[[2]], split = "[+]"))) else NULL
  return(list(DLT.name = DLT.name,
              npat.name = npat.name,
              drug.name = drug.name,
              covariates = covariates))
}
