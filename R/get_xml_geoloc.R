#' Retrieve the geolocation coordinates of a node from an XML node
#'
#' @param xml_node A XML node of a SIC node or section
#'
#' @returns A numeric vector with the geolocation coordinates of the node
#' @export
#'
get_xml_geoloc <- function(xml_node) {
  stopifnot(
    inherits(xml_node, "xml_node"),
    xml2::xml_name(xml_node) %in% c("Noeud", "SectionMin")
  )

  # Get the coordinates of the node
  x_geo <- xml_node |> xml2::xml_find_first("Aff/Xgeo") |> xml2::xml_text() |> as.numeric()
  y_geo <- xml_node |> xml2::xml_find_first("Aff/Ygeo") |> xml2::xml_text() |> as.numeric()

  return(c(x_geo = x_geo, y_geo = y_geo))
}
