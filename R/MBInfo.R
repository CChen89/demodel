#' Input information about model-based design
#'
#' @param dose.levels A named list. names must match drug names.
#' @param ref.dose A vector of numeric value.
#' @param bounds A vector of two numeric values. Cutoff points of two categories. By the default, (0.16, 0.33).
#' @param ewoc A numeric value. Escalation with overdosing control. The default is 0.25.
#' @param trial.name A string.
#' @param drug.name A vector of string.
#' @param drug.unit A vector of string.
#' @return A named list
#' @export
#' @examples
#'
#' \dontrun{
#'   MB.Info <- MBInfo(dose.levels = list(Drug = c(80, 120, 160, 200, 240, 280)),
#'                     ref.dose = 120,
#'                     bounds = c(0.16, 0.33),
#'                     ewoc = 0.25,
#'                     trial.name = "Drug666",
#'                     drug.name = "DrugX",
#'                     drug.unit = "mg QD")
#' }

MBInfo <- function(dose.levels = NULL, ref.dose = NULL, bounds = c(0.16, 0.33), ewoc = 0.25, trial.name = NULL, drug.name = NULL, drug.unit = NULL)
{
  return(list(dose.levels = dose.levels,
              ref.dose = ref.dose,
              bounds = bounds,
              ewoc = ewoc,
              trial.name = trial.name,
              drug.name = drug.name,
              drug.unit = drug.unit))
}
