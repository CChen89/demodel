#' @title A hypothetical single-agent trial data
#'
#' @description A simple \code{data.frame} that is used for testing and running of examples
#'
#' @docType data
#'
#' @usage data(blrm_mono_data)
#'
#' @keywords datasets
#'
#' @format A cohort-wise trial data:
#' \describe{
#'   \item{Scenario}{Hypothetical scenarios}
#'   \item{DSPX}{doses tested for every cohort, separatded by comma}
#'   \item{npat}{the number of patients treated for every cohort, separated by comma}
#'   \item{nDLT}{the number of DLTs observed for every cohort, separated by comma}
#'   ...
#' }
#' @examples
#' data(blrm_mono_data)
#' blrm_mono_data
"blrm_mono_data"
