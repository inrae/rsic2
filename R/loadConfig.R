#' Read default configuration of the package and complete it with eventual user config
#'
#' @param userFile location of the user config YML file
#' @param pathDefaultCfg The location of the default configuration (located in "inst/config.yml" of the package by default)
#'
#' @return A configuration as it is returned by [config::get].
#'
#' Configuration of RSIC2 as the following structure:
#'
#' - `sic`
#'   - `path`: Path of local SIC installation (should be defined by `sic_path` parameter or in the user config file)
#'   - `edisic`: sub-path to EdiSIC program
#'   - `talweg`: sub-path to TALWEG program
#'   - `fluvia`: sub-path to FLUVIA program
#'   - `sirene`: sub-path to SIRENE program
#'   - `export`: sub-path to SicExport program
#'   - `fortran`:
#'     - `prms`:
#'       - `INTERF`: default `INTERF` parameter injected in command line arguments of TALWEG, FLUVIA, SIRENE
#' - `project`
#'   - `path`: Path to the XML project file (should be defined by `xml_path` parameter or in the user config file)
#'
#' @export
#'
#' @examples
#' library(rsic2)
#' sic_path <- tempdir(check = TRUE)
#' xml_path <- R.utils::tmpfile()
#' cfg <- loadConfig(sic_path, xml_path)
#' str(cfg)
loadConfig <- function(sic_path = NULL, xml_path = NULL, userFile = "config.yml", pathDefaultCfg = system.file("config.yml", package = "rsic2")) {
    cfg <- config::get(file = pathDefaultCfg)
    if (file.exists(userFile)) {cfg = config::merge(cfg,config::get(file = userFile))}
    if (!is.null(sic_path)) cfg$sic$path <- sic_path
    if (!is.null(xml_path)) cfg$project$path <- xml_path
    if (!dir.exists(cfg$sic$path)) {
      stop("Path for SIC is undefined or does not exist: ", cfg$sic$path)
    }
    if (!file.exists(cfg$project$path)) {
      stop("Path for XMLproject file is undefined or the file does not exist: ", cfg$project$path)
    }
    cfg
}
