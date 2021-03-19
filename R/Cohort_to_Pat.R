Cohort_to_Pat <- function(cumu.data, ref.dose, variable.names)
{
  ###########################################################
  # cumu.data: a data.frame, cumulative cohort data
  # ref.dose: a scalar (mono) or a vector of length 2 (combo)
  ###########################################################

  DLT.name <- variable.names$DLT.name
  npat.name <- variable.names$npat.name
  drug.name <- variable.names$drug.name
  covariates <- variable.names$covariates

  if(!all(cumu.data[[npat.name]] >= cumu.data[[DLT.name]])) stop("# of DLTs cannot be larger than npat")

  log.dose.ratio.names <- paste("log", drug.name, "dose", "ratio", sep = ".")
  dose.data <- cumu.data[, .SD, .SDcols = c(npat.name, drug.name)]

  cohort.DLT <- cumu.data[, list(DLT = c(rep(1, .SD[[DLT.name]]), rep(0, .SD[[npat.name]]-.SD[[DLT.name]]))), by = seq_len(nrow(cumu.data)), .SDcols = c(DLT.name, npat.name)][, "DLT"] %>% setnames("DLT", DLT.name)

  dose.data <- dose.data[, c(log.dose.ratio.names) := lapply(1:length(drug.name), function(i) log(.SD[[i]]/ref.dose[i])), .SDcols = drug.name][rep(1:.N, get(npat.name))][, .SD, .SDcols = c(log.dose.ratio.names)]

  Pat.data <- cbind(cohort.DLT, dose.data)

  return(Pat.data)
}
