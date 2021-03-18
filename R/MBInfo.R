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
