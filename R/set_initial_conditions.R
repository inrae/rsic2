#' Import initial conditions
#'
#' Import initial conditions from a scenario/variant result calculation to a new scenario/variant.
#'
#' @param iniParams [numeric], arguments passed to Edisic for importing initial conditions. See details
#' @template param_cfg
#'
#' @details `iniParams` [numeric] vector of length 5. Each number correspond to:
#'
#' 1. number of the scenario from which import initial conditions
#' 2. number of the variant from which import initial conditions (Use "0" for no variant)
#' 3. Time in seconds of the initial conditions to import (Use "0" for a single permanent simulation with no time steps)
#' 4. number of the scenario where the initial conditions are exported to
#' 5. number of the variant where the initial conditions are exported to  (Use "0" for no variant)
#'
#' @export
#'
#' @inherit sic_run_mesh return examples
set_initial_conditions <- function(iniParams, cfg = loadConfig()) {
  if (!is.numeric(iniParams) || length(iniParams) != 5) {
    stop("`iniParams` should be a numeric of length 5")
  }
  sArgs <- paste(iniParams, collapse = " ")
  cmd_line <- shQuote(
    paste(
      file.path(cfg$sic$path, cfg$sic$edisic),
      shQuote(dirname(cfg$project$path), type = "cmd"),
      "CI",
      shQuote(basename(cfg$project$path), type = "cmd"),
      sArgs
    ),
    type = "cmd2"
  )
  logger::log_debug(cmd_line)
  shell(
    cmd_line,
    wait = T,
    translate = T
  )
}
