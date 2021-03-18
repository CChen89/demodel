#' Input parameters about output report
#'
#' @param core A integer indicating how many cores will be used for parallel computation. The default uses all available cores.
#' @param date The default is the the date when one run this code
#' @param code.name the R file name used for \code{\link{demodelFit}} function. The default is 'demodel.R'. Can be modified for version control.
#' @param fig.height,fig.width  A integer. The height and width of figures attached in the final report. The default is 8 for both width and height. See \code{\link[ggplot2]{ggsave}} for more information.
#' @param out.path A string. Folder location where the report is saved
#' @param table.file.name A string with suffix .xlsx. The file name of final report.
#' @param fig.file.name deprecated. Will be deleted later.
#' @return A named list
#' @export
#' @examples
#'
#' \dontrun{
#'
#' demodelControl(core = 8,
#'                date = format(Sys.Date(), "%Y-%m-%d"),
#'                code.name = "demodel_core.R",
#'                output.path = getwd(),
#'                table.file.name = "preview.xlsx",
#'                fig.file.name = NULL)
#'
#'
#' }

demodelControl <- function(core = parallel::detectCores(), date = format(Sys.Date(), "%Y-%m-%d"), code.name = NULL, fig.height = 8, fig.width = 8, output.path = getwd(), table.file.name = NULL, fig.file.name = NULL)
{
 return(list(core = core,
             date = date,
             code.name = code.name,
             fig.height = fig.height,
             fig.width = fig.width,
             table.path = if(is.null(table.file.name)) NULL else paste0(output.path, "/", table.file.name),
             fig.path = if(is.null(fig.file.name)) NULL else paste0(output.path, "/", fig.file.name)))
}
