#' Run Talweg, Fluvia or Sirene for a configured project
#'
#' Use `sic_run_mesh` to run the mesh generator, `sic_run_steady` for steady flow
#' simulation, and `sic_run_unsteady` for unsteady flow simulation.
#'
#' @param scenario [numeric], the scenario to use
#' @param variant [numeric], the variant to use (0 by default means no variant)
#' @param params [list] or [character], see details
#' @param sicInputs A [SicInput] object or a list of [SicInput] objects create by [merge.SicInput]
#'                  used to create a PAR file injectig inputs for the simulation
#' @param iniParams 5-length [numeric] [vector], see [set_initial_conditions] for details
#' @template param_cfg
#'
#' @details The argument `params` handles the parameters describe in
#' [SIC documentation](https://sic.g-eau.fr/Execution-de-TALWEG-FLUVIA-et).
#' If argument `params` is a [list], arguments are injected in the command line
#' by taking the items of the list with the conversion `[key]=[value]`.
#' By default, the parameter `INTERF=0` is added to the command line.
#' If argument `params` is a [character] string it is directly used as parameters of the command line.
#'
#' @return Error code returned by [shell].
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up the configuration model
#' cfg <- cfg_tmp_project()
#'
#' # Generate the mesh of the model
#' sic_run_mesh(cfg)
#'
#' # Run steady simulation for the scenario #1
#' sic_run_steady(cfg, scenario = 1)
#'
#' # Import initial condition from scenario 1
#' # to scenario 1, variant 1 for unsteady flow simulation
#' set_initial_conditions(c(1, 0, 0, 1, 1), cfg = cfg)
#'
#' # Run unsteady flow simulation
#' sic_run_unsteady(cfg, scenario = 1, variant = 1)
#'
#' # Or initiate and run the same unsteady flow simulation in one call
#' sic_run_unsteady(cfg, iniParams = c(1, 0, 0, 1, 1))
#' }
sic_run_mesh <- function(cfg, params = list()) {
  sic_run_fortran(cfg, "talweg", params)
}

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
#' @noRd
#'
#' @examples
#' \dontrun{
#' cfg <- cfg_tmp_project()
#' convert_sic_params(list(SCE = 1, VAR = 1), cfg = cfg)
#' }
convert_sic_params <- function(params, cfg = loadConfig()) {
  if (!"INTERF" %in% names(params)) {
    params <- c(list(INTERF = cfg$sic$fortran$prms$INTERF), params)
  }
  params <- sapply(names(params), function(key) {
    paste(key, params[[key]], sep = "=")
  })
  paste(params, collapse = " ")
}

#' @param prog [character], the program to run. Should be one of "talweg"
#' @noRd
sic_run_fortran <- function(cfg, prog, params) {
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
  shell(
    cmd_line,
    wait = T,
    translate = T
  )
}

#' @noRd
sic_run_simulation <- function(
  cfg,
  prog,
  scenario,
  variant,
  sicInputs,
  params
) {
  if (!is.null(sicInputs)) {
    sic_write_par(cfg, scenario, sicInputs)
  }
  params <- c(list(SCE = scenario, VAR = variant), params)
  sic_run_fortran(cfg, prog, params)
}

#' @rdname sic_run_mesh
#' @export
sic_run_steady <- function(
  cfg,
  scenario,
  variant = 0,
  sicInputs = NULL,
  params = list()
) {
  sic_run_simulation(cfg, "fluvia", scenario, variant, sicInputs, params)
}

#' @rdname sic_run_mesh
#' @export
sic_run_unsteady <- function(
  cfg,
  scenario = iniParams[4],
  variant = iniParams[5],
  sicInputs = NULL,
  iniParams = NULL,
  params = list()
) {
  if (!is.null(iniParams)) {
    sic_run_steady(
      cfg,
      scenario = iniParams[1],
      variant = iniParams[2],
      sicInputs = sicInputs
    )
    set_initial_conditions(iniParams, cfg)
  }
  if (is.null(scenario)) stop("`scenario` should be defined")
  if (is.null(variant)) variant <- 0
  sic_run_simulation(cfg, "sirene", scenario, variant, sicInputs, params)
}
