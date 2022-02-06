#' Convert a list of parameters into command line arguments
#'
#' @param params a [list] of [character] with each item is identified by
#' @param cfg
#'
#' @return A [character] with each parameter is converted to "[key param]=[value param]" and each parameter separated by a space character.
#' @export
#'
#' @examples
convert_sic_params <- function(params, cfg = loadConfig()) {
  if (!"INTERF" %in% names(params)) {
    params <- c(list(INTERF = cfg$sic$fortran$prms$INTERF), params)
  }
  params <- sapply(names(params), function(key) {
    paste(key, params[[key]], sep= "=")
  })
  paste(params, collapse = " ")
}
