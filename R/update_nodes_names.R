#' Update nodes names in a SIC model XML file
#'
#' @param new_names [character], new names for the nodes in the SIC model
#' @template param_cfg
#'
#' @returns Nothing. Use for side effect on the XML project file.
#' @export
#'
update_nodes_names <- function(new_names, cfg) {
  # Check if new_names is a character vector
  if (!is.character(new_names)) {
    stop("new_names must be a character vector")
  }

  # Write the updated XML file
  xp <- xml2::read_xml(cfg$project$path)
  xPath_template <- "/Reseau/Liste_Noeuds/Noeud"
  xml_nodes <- xml2::xml_find_all(xp, xPath_template)

  # Check if new_names has the same length as the number of nodes
  if (length(new_names) != length(xml_nodes)) {
    stop("new_names must have the same length as the number of nodes")
  }

  # Update node names in the XML file
  for (i in seq_along(new_names)) {
    xml2::xml_attr(xml_nodes[[i]], "Nom") <- new_names[i]
  }

  xml2::write_xml(xp, cfg$project$path)
}
