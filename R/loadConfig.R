#' Read default configuration of the package and complete it with eventual user config
#'
#' @param sic_path [character], the path to an installation of SIC (see details)
#' @param xml_path [character], the path of the XML project file (see details)
#' @param userFile location of the user config YML file
#' @param pathDefaultCfg The location of the default configuration (located in "inst/config.yml" of the package by default)
#'
#' @details
#' The path to SIC and to the XML project file can be defined in the user XML file by defining the appropriate items:
#'
#' ```yaml
#' sic:
#'   path: Path/To/SIC/installation
#' project:
#'   path: Path/To/Xml/Project/File.xml
#' ```
#'
#' Moreover, the `sic_path` can be defined with the environment variable "SICPATH".
#' This setting is a default which is overwritten by the path defined in the user YAML file,
#' which is also overwritten by the one define by the `sic_path` argument.
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
#'   - `stricklers`: A vector of 2 [numeric] values representing respectively
#'     the default minor and medium bed Strickler coefficients to apply with [sic_import_reaches]
#'
#' @export
#'
#' @examples
#' library(rsic2)
#' sic_path <- tempdir(check = TRUE)
#' xml_path <- R.utils::tmpfile()
#' cfg <- loadConfig(sic_path, xml_path)
#' str(cfg)
loadConfig <- function(
  sic_path = NULL,
  xml_path = NULL,
  new_project = FALSE,
  userFile = "config.yml",
  pathDefaultCfg = system.file("config.yml", package = "rsic2")
) {
  cfg <- config::get(file = pathDefaultCfg)
  if (Sys.getenv("SICPATH") != "" & is.null(sic_path)) {
    cfg$sic$path <- Sys.getenv("SICPATH")
    message("`sic_path` defined by environment variable SICPATH=", cfg$sic$path)
  }
  if (file.exists(userFile)) {
    message("Reading user configuration from ", userFile)
    cfg = config::merge(cfg, config::get(file = userFile))
  }
  if (!is.null(sic_path)) cfg$sic$path <- sic_path
  if (!is.null(xml_path)) cfg$project$path <- xml_path
  if (!dir.exists(cfg$sic$path)) {
    stop("Path for SIC is undefined or does not exist: ", cfg$sic$path)
  }
  if (!new_project && !file.exists(cfg$project$path)) {
    stop(
      "Path for XMLproject file is undefined or the file does not exist: ",
      cfg$project$path
    )
  }
  cfg$project$new <- new_project
  cfg
}
