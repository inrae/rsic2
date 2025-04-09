#' Add coordinates on nodes in a SIC project XML file
#'
#' @param x,y [numeric], coordinates of the nodes in the SIC project
#' @param new_name [character] [vecto] of replacement names for the nodes
#' @template param_cfg
#'
#' @returns Nothing. Use for side effect on the XML project file.
#' @export
#'
add_node_geoloc <- function(x, y, new_names = NULL, cfg) {
  xp <- xml2::read_xml(cfg$project$path)
  xPath_template <- "/Reseau/Liste_Noeuds/Noeud[%i]"

  for (i in seq_along(x)) {
    xPath <- sprintf(xPath_template, i)
    node <- xp %>% xml2::xml_find_first(xPath)
    if (!is.null(new_names)) {
      xml2::xml_attr(node, "Nom") <- new_names[i]
    }
    nodeX <- node |> xml2::xml_child("Aff") |> xml2::xml_child("Xgeo")
    xml2::xml_text(nodeX) <- as.character(x[i])
    nodeY <- node |> xml2::xml_child("Aff") |> xml2::xml_child("Ygeo")
    xml2::xml_text(nodeY) <- as.character(y[i])
  }
  xml2::write_xml(xp, cfg$project$path)
}
