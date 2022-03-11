#' Convert a list of parameters into command line arguments for Fortran SIC programs
#'
#' This function is called by [sic_run_fortran] to convert list of parameters into a [character] command line parameters.
#'
#' The parameter `INTERF` is set to 0 (zero) by default.
#'
#' @param params a [list] of [character] containing the parameters to send to the fortran program with format `list(param=value, ...)`
#' @template param_cfg
#'
#' @return A [character] with each parameter is converted to `[key param]=[value param]` and each parameter separated by a space character.
#' @export
#'
#' @examples
#' convert_sic_params(list(SCE = 1, VAR = 1))
#'
convert_sic_params <- function(params, cfg = loadConfig()) {
  if (!"INTERF" %in% names(params)) {
    params <- c(list(INTERF = cfg$sic$fortran$prms$INTERF), params)
  }
  params <- sapply(names(params), function(key) {
    paste(key, params[[key]], sep= "=")
  })
  paste(params, collapse = " ")
}
