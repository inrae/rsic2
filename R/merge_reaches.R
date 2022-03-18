#' Merge several *ReachTxt* objects into one
#'
#' @param ... *ReachTxt* objects
#'
#' @return a *ReachTxt* object (See [create_uniform_reach_txt] and [dem_to_reach]) containing the merged reaches.
#' @export
#'
#' @examples
#' # Minor bed generation
#' profT <- list(
#'   B = 2,
#'   S = (6 - 2) / 2 / 2,
#'   ZF = 100,
#'   ZB = 100 + 2
#' )
#' min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
#'                          upstream_bed_elevation = 10 + 2000 * 0.002,
#'                          slope = 0.002,
#'                          section_type = "T",
#'                          profile = profT)
#'
#' # Major bed generation
#' data("floodam_ead_dem")
#' dem <- terra::rast(floodam_ead_dem)
#' node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
#' space_step = 100
#' section_width = 5000
#' maj_reach <- dem_to_reach_txt(dem, node_coords, space_step, section_width, major_bed = TRUE)
#'
#' # Merge reaches into one
#' reach <- merge_reaches(min_reach, maj_reach)
merge_reaches <- function(...) {
  reaches <- list(...)
  lapply(reaches, function(reach) {
    if (!inherits(reach, "ReachTxt")) stop("Parameters must be of class ReachTxt")
  })
  if (length(reaches) == 1) return(reaches[[1]])
  merged_reach <- do.call(c, reaches)
  merged_reach <- merged_reach[order(names(merged_reach))]
  class(merged_reach) <- c("ReachTxt", class(merged_reach))
  return(merged_reach)
}
