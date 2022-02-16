#' Run SicExport and read the exported file
#'
#' @param scenario [numeric], the scenario to read
#' @param variant [numeric], the variant to read
#' @param params [character] location parameters of the result. See \url{https://sic.g-eau.fr/sicexport-utilitaire-d-exportation} for details.
#' @template param_cfg
#'
#' @return [matrix] with the read result
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(SCE=1)
#' sic_run_fortran("fluvia", params)
#' # For exporting result in sections at time 0
#' sic_run_export(scenario = 1, params = "/t=0")
#' }
sic_run_export <- function(scenario, variant = 0, params, cfg = loadConfig()) {
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
