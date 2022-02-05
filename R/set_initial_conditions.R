#' Import initial conditions
#'
#' Import initial conditions from a scenario/variant result calculation to a new scenario/variant.
#'
#' @param params [numeric], arguments passed to Edisic for importing initial conditions. See details
#' @param cfg
#'
#' @details `params` [numeric] vector of length 5. Each number correspond to:
#'
#' 1. number of the scenario from which import initial conditions
#' 1. number of the variant from which import initial conditions (Use "0" for no variant)
#' 1. Time in seconds of the initial conditions to import (Use "0" for a single permanent simulation with no time steps)
#' 1. number of the scenario where the initial conditions are exported to
#' 1. number of the variant where the initial conditions are exported to  (Use "0" for no variant)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # Import initial conditions from the scenario #1 without variant at time 0s
#' # to the variant #1 in the scenario #1
#' set_initial_conditions("1 0 0 1 1")
#' }
set_initial_conditions <- function(params, cfg = loadConfig()) {
  if (!is.numeric(params) || length(params) != 5) {
    stop("`params` should be a numeric of length 5")
  }
  sArgs <- paste(params, collapse = " ")
  shell(
    shQuote(
      paste(
        file.path(cfg$sic$path, cfg$sic$edisic),
        shQuote(dirname(cfg$project$path), type = "cmd"),
        "CI",
        shQuote(basename(cfg$project$path), type = "cmd"),
        sArgs
      ),
      type = "cmd2"
    ),
    wait = T,
    translate = T
  )
}
