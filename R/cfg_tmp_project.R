#' Set a configuration with a temporary project directory
#'
#' @param xml_path [character], the path of the XML SIC project file
#' @param cfg [config], the configuration to modify
#'
#' @return The updated configuration with the temporary project directory
#' @export
#'
#' @examples
#' cfg <- cfg_tmp_project()
#' cfg$project$xml_path
#'
cfg_tmp_project <- function(xml_path = system.file("sic_project_test1.xml", package = "rsic2"), cfg = loadConfig(xml_path = xml_path)) {
  cfg$project$path <- tempfile("sic_project", fileext = ".xml")
  file.copy(xml_path,
            cfg$project$path,
            overwrite = TRUE)
  return(cfg)
}
