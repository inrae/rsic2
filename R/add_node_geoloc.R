#' Add coordinates on nodes in a SIC project XML file
#'
#' @param x,y [numeric], coordinates of the nodes in the SIC project
#' @param display_xy [data.frame] with 2 rows and 3 columns `num`, `x`, and `y`
#' @template param_cfg
#'
#' @returns Nothing. Use for side effect on the XML project file.
#' @export
#'
add_node_geoloc <- function(x, y, display_xy = NULL, cfg) {
  xp <- xml2::read_xml(cfg$project$path)
  xPath_template <- "/Reseau/Liste_Noeuds/Noeud[%i]"

  if (!is.null(display_xy)) {
    scale_x <- (display_xy[2, "x"] - display_xy[1, "x"]) /
      (x[display_xy[2, "num"]] - x[display_xy[1, "num"]])
    scale_y <- (display_xy[2, "y"] - display_xy[1, "y"]) /
      (y[display_xy[2, "num"]] - y[display_xy[1, "num"]])
    origin <- list(
      x = list(display = display_xy[1, "x"], geo = x[display_xy[1, "num"]]),
      y = list(display = display_xy[1, "y"], geo = y[display_xy[1, "num"]])
    )
  }
  if (!is.null(display_xy)) {
    x_display <- as.integer((x - origin$x$geo) * scale_x + origin$x$display)
    y_display <- as.integer((y - origin$y$geo) * scale_y + origin$y$display)
  }

  for (i in seq_along(x)) {
    xPath <- sprintf(xPath_template, i)
    node <- xp %>% xml2::xml_find_first(xPath)
    nodeX <- node |> xml2::xml_child("Aff") |> xml2::xml_child("Xgeo")
    xml2::xml_text(nodeX) <- as.character(x[i])
    nodeY <- node |> xml2::xml_child("Aff") |> xml2::xml_child("Ygeo")
    xml2::xml_text(nodeY) <- as.character(y[i])
    if (!is.null(display_xy)) {
      nodeX <- node |> xml2::xml_child("Aff") |> xml2::xml_child("X")
      xml2::xml_text(nodeX) <- as.character(x_display[i])
      nodeY <- node |> xml2::xml_child("Aff") |> xml2::xml_child("Y")
      xml2::xml_text(nodeY) <- as.character(y_display[i])
    }
  }

  if (!is.null(display_xy)) {
    # Relocate middle position of the reaches
    xPath <- "/Reseau/Liste_Biefs/Bief"
    lapply(
      xml2::xml_find_all(xp, xPath),
      function(xml_bief) {
        # Get the coordinates of the upstream and downstream nodes
        coords <- calc_middle_reach_display_coords(xp, xml_bief)
        # Set the coordinates in the XML file
        nodeX <- xml_bief |>
          xml2::xml_child("Aff") |>
          xml2::xml_child("X_Millieu")
        xml2::xml_text(nodeX) <- as.character(coords[1])
        nodeY <- xml_bief |>
          xml2::xml_child("Aff") |>
          xml2::xml_child("Y_Millieu")
        xml2::xml_text(nodeY) <- as.character(coords[2])
      }
    )
  }
  xml2::write_xml(xp, cfg$project$path)
}

calc_middle_reach_display_coords <- function(xp, xml_bief) {
  up_node_id <- xml_bief |>
    xml2::xml_find_first("Top/NoeudAm") |>
    xml2::xml_text()
  up_node_coords <- get_node_display_coords(xp, up_node_id)
  down_node_id <- xml_bief |>
    xml2::xml_find_first("Top/NoeudAv") |>
    xml2::xml_text()
  down_node_coords <- get_node_display_coords(xp, down_node_id)
  x_display <- (up_node_coords[1] + down_node_coords[1]) / 2
  y_display <- (up_node_coords[2] + down_node_coords[2]) / 2
  return(as.integer(c(x_display, y_display)))
}

get_node_display_coords <- function(xp, node_id) {
  xPath <- sprintf("/Reseau/Liste_Noeuds/Noeud[@Num=%s]/Aff", node_id)
  node <- xp %>% xml2::xml_find_first(xPath)
  x_display <- node |> xml2::xml_child("X") |> xml2::xml_text()
  y_display <- node |> xml2::xml_child("Y") |> xml2::xml_text()
  return(as.integer(c(x_display, y_display)))
}
