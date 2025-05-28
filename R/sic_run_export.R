#' Run SicExport and read the exported file
#'
#' @details
#' `params` parameter is a list representing parameters available in \url{https://sic.g-eau.fr/sicexport-utilitaire-d-exportation} to set the model network location of exported results. The string parameter `/x=n /yy=ii` in the command line is here represented by `list(xxx = nnn, yy = ii)`.
#' @inheritParams sic_run_mesh
#' @param params [list] location parameters of the result, see details.
#' @template param_cfg
#'
#' @return [matrix] with the read result
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(SCE=1)
#' sic_run_steady(cfg, scenario = 1)
#' # For exporting result in sections at time 0
#' sic_run_export(scenario = 1, params = list(t = 0))
#' }
sic_run_export <- function(scenario, variant = 0, params, cfg = loadConfig()) {
  if (is.list(params)) params <- unlist(params)
  params <- paste(paste0("/", names(params)), params, sep = "=", collapse = " ")

  file <- tempfile("sicexport", fileext = ".tsv")
  cmd_line <-
    paste(
      shQuote(file.pathwin(cfg$sic$path, cfg$sic$export), type = "cmd"),
      shQuote(cfg$project$path, type = "cmd"),
      paste0("/sce=", scenario),
      paste0("/var=", variant),
      params,
      "/quiet=1",
      paste0("/out=", shQuote(file, type = "cmd"))
    )
  logger::log_debug(cmd_line)
  shell(
    shQuote(cmd_line, type = "cmd2"),
    wait = T,
    translate = F
  )
  as.matrix(read.csv(file, sep = "\t"))
}
