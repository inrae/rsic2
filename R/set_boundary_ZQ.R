#' Set stage-discharge curve at an offtake of the model
#'
#' @description
#' Update the existing stage-discharge boundary condition.
#'
#' @inheritParams sic_run_mesh
#' @param nd [numeric], node number in SIC model
#' @param pr [numeric], offtake number in the node
#' @param mZQ [matrix], stage-discharge relation with first column the water elevation in meters
#'            and the second column the discharge in cubic meters per seconds
#'
#' @return This function is only used for side effect on the XML file of the SIC project.
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- cfg_tmp_project()
#' z <- seq(5, 20, 1)
#' Q <- (z-5)^1.5
#' set_boundary_ZQ(cfg, scenario = 1, nd = 3, mZQ = matrix(c(z, Q), ncol = 2))
#' }
set_boundary_ZQ <- function(cfg, scenario, nd, pr = 1, mZQ) {
  x <- read_xml(cfg$project$path)
  xPath <- "/Reseau/Liste_Noeuds/Noeud[@Num=%d]/Flu[@nScenario=%d]/Prise[@Num=%d]/ConditionLim/TLoi"
  xPath <- sprintf(xPath, nd, scenario, pr)
  sZQ <- capture.output(write.table(
    t(mZQ),
    sep = "\t",
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE
  ))
  sZQ <- paste(sZQ, collapse = "\n")
  xLoi <- x %>% xml_find_first(xPath)
  xml_text(xLoi) <- sZQ
  write_xml(x, cfg$project$path)
}
