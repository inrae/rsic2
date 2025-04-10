#' Retrieve node or reach number from XML project file
#'
#' @param name [character] name of the node or reach
#' @param xp [xml2::xml_document] the XML project file. Default is the one in the configuration
#' @template param_cfg
#'
#' @returns [integer] the number of the node or reach in the XML project file
#' @rdname get_xml_num_by_name
#' @export
#'
#' @examples
#' xp <- xml2::read_xml(system.file("sic_project_test1.xml", package = "rsic2"))
#' get_xml_node_num_by_name("V1-droite", xp)
#' get_xml_reach_num_by_name("Bief3", xp)
get_xml_node_num_by_name <- function(name, xp = xml2::read_xml(cfg$project$path), cfg) {
  # Get the node number
  xPath <- sprintf("/Reseau/Liste_Noeuds/Noeud[@Nom='%s']", name)
  node <- xp %>% xml2::xml_find_first(xPath)
  if (is.na(node)) {
    stop(sprintf("Node with name '%s' not found in the XML project file.", name))
  }
  node_num <- xml2::xml_attr(node, "Num")
  return(as.integer(node_num))
}

#' @rdname get_xml_num_by_name
#' @export
get_xml_reach_num_by_name <- function(name, xp = xml2::read_xml(cfg$project$path), cfg) {
  # Get the node number
  xPath <- sprintf("/Reseau/Liste_Biefs/Bief[@Nom='%s']", name)
  node <- xp %>% xml2::xml_find_first(xPath)
  if (is.na(node)) {
    stop(sprintf("Reach with name '%s' not found in the XML project file.", name))
  }
  node_num <- xml2::xml_attr(node, "Num")
  return(as.integer(node_num))
}

#' @title Get reach number from node number
#'
#' @param node_num [numeric] node number in the SIC model
#' @param upstream [logical] if TRUE, get the reach number from the upstream node, otherwise downstream
#' @inheritParams get_xml_node_num_by_name
#'
#' @returns [integer] the reach number in the XML project file
#' @export
#'
#' @examples
#' xp <- xml2::read_xml(system.file("sic_project_test1.xml", package = "rsic2"))
#' get_xml_reach_num_by_node_num(1, upstream = TRUE, xp)
#' get_xml_reach_num_by_node_num(2, upstream = FALSE, xp)
get_xml_reach_num_by_node_num <- function(node_num, upstream = TRUE, xp = xml2::read_xml(cfg$project$path), cfg) {
  # Get the reach number
  updown_string <- ifelse(upstream, "Am", "Av")
  xPath <- sprintf("/Reseau/Liste_Biefs/Bief/Top/Noeud%s[text()=%i]", updown_string, as.integer(node_num))
  node <- xp %>% xml_find_first(xPath)
  if (is.na(node)) {
    stop(sprintf("Reach with node number '%d' not found in the XML project file.", node_num))
  }
  reach_num <- node |> xml2::xml_parent() |> xml2::xml_parent() |> xml_attr("Num")
  return(as.integer(reach_num))
}
