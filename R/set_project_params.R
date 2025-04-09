#' Set project params in the SIC project XML file
#'
#' @param params [list] of parameters (See details)
#' @template param_cfg
#'
#' @details
#' The parameter `params` is a list of parameters to set in the SIC project XML file.
#' The list should be named with the parameter name.
#' Possible values of parameters are:
#'
#' - `HomothetieX`
#' - `HomothetieY`
#' - `DX`
#' - `ImportEdital`
#' - `GeoUnit`
#' - `GeoRefAngle`
#'
#' @returns None. Use for side effect on the XML project file.
#' @export
#'
set_project_params <- function(params, cfg) {
  xp <- xml2::read_xml(cfg$project$path)
  xPath <- "/Reseau/Tal"

  for (id in names(params)) {
    node <- xp %>% xml2::xml_find_first(file.path(xPath, id))
    xml2::xml_text(node) <- as.character(params[[id]])
  }
  xml2::write_xml(xp, cfg$project$path)
}
