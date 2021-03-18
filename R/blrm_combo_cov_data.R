#' @title A hypothetical dual-agent trial data with covariates
#'
#' @description A simple \code{data.frame} that is used for testing and running of examples
#'
#' @docType data
#'
#' @usage data(blrm_combo_cov_data)
#'
#' @keywords datasets
#' @format A patient-wise trial data:
#' \describe{
#'   \item{Scenario}{Hypothetical scenarios}
#'   \item{DSPX1}{dose of drug1 tested for every patient}
#'   \item{DSPX2}{dose of drug2 tested for every patient}
#'   \item{DLT}{If DLT was observed for every patient}
#'   \item{Azole}{A dummy variable}
#'   ...
#' }
#' @examples
#' data(blrm_combo_cov_data)
#' blrm_combo_cov_data
"blrm_combo_cov_data"
