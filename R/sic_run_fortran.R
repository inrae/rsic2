#' Run Talweg, Fluvia or Sirene
#'
#' @param prog [character], the program to run. Should be one of "talweg"
#' @param params [list] or [character], see details
#' @template param_cfg
#'
#' @details If argument `params` is a [list], arguments are injected in the command line by taking the items of the list with the conversion
#' `[key]=[value]`. If argument `params` is a [character]
#'
#' @return Error code returned by [shell].
#' @export
#'
#' @examples
#' \dontrun{
#' # Run steady simulation for the scenario #1
#' cfg <- cfg_tmp_project()
#' params <- list(SCE=1)
#' sic_run_fortran("fluvia", params, cfg = cfg)
#'}
sic_run_fortran <- function(prog, params = list(), cfg = loadConfig()) {
  if (is.list(params)) params <- convert_sic_params(params, cfg)
  cmd_line <- shQuote(
    paste(
      file.path(cfg$sic$path, cfg$sic[[prog]]),
      shQuote(cfg$project$path, type = "cmd"),
      params
    ),
    type = "cmd2"
  )
  logger::log_debug(cmd_line)
  ret <- shell(
    cmd_line,
    wait = T,
    translate = T
  )
  file.remove("FLUVIA.INI", "SIRENE.INI")
  return(ret)
}
