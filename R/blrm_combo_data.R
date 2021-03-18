#' @title A hypothetical Dual-agent trial data
#'
#' @description A simple \code{data.frame} that is used for testing and running of examples
#'
#' @docType data
#'
#' @usage data(blrm_combo_data)
#'
#' @keywords datasets
#' @format A cohort-wise trial data:
#' \describe{
#'   \item{Scenario}{Hypothetical scenarios}
#'   \item{DSPX1}{doses of drug1 tested for every cohort, separatded by comma}
#'   \item{DSPX2}{doses of drug2 tested for every cohort, separated by comma}
#'   \item{npat}{the number of patients treated for every cohort, separated by comma}
#'   \item{nDLT}{the number of DLTs observed for every cohort, separated by comma}
#'   ...
#' }
#' @examples
#' data(blrm_combo_data)
#' blrm_combo_data
"blrm_combo_data"
