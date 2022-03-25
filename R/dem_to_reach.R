#' Create SIC geometry sections from a DEM
#'
#' The coordinate system of `dem` should be a metric orthonormal coordinate system.
#'
#' @param dem [terra::SpatRaster] object with altitude data (m).
#' @param node_coords a 2x2 [matrix] with coordinates of starting and ending points of the line to browse
#' @param space_step 1-length [numeric], distance between each section (m)
#' @param section_width 1-length [numeric], width of the sections to create
#' @param nb_points 1-length [numeric], number of points to describe cross-section geometries
#' @param start 1-length [numeric], starting value for the chainage (i.e. section abscissa) along the reach
#' @param major_bed [logical], `TRUE` for major bed, `FALSE` for minor-medium bed
#'
#' @return A *ReachTxt* object which is a [list] of *SectionTxt* objects (see [create_section_txt]).
#' @rdname dem_to_reach
#' @export
#' @importFrom stats dist
#'
#' @examples
#' ## Inputs preparation
#' data("floodam_ead_dem")
#' dem <- terra::rast(floodam_ead_dem)
#' node_coords <- matrix(c(102550, 102550, 110000, 108000), ncol = 2)
#' space_step = 100
#' section_width = 5000
#'
#' ## Get section positions
#' reach_length <- as.numeric(dist(node_coords[1:2,]))
#' nb_sections <- ceiling(reach_length / space_step) + 1
#' section_centers <- get_section_centers(node_coords, nb_sections)
#'
#' ## Create a section profile and plot it!
#' profile <- dem_to_section(dem, node_coords, section_centers[5,], section_width)
#' plot(profile)
#'
#' ## Generate cross section profiles for a reach
#' reach <- dem_to_reach(dem, node_coords, section_centers, section_width)
#' plot(reach[[5]])
#'
#' ## Generate section profiles in SIC import text format
#' reach_txt <- dem_to_reach_txt(dem, node_coords, space_step, section_width, major_bed = TRUE)
#' reach_txt[[5]]
#'
dem_to_reach_txt <- function(dem, node_coords, space_step, section_width, nb_points = 50, start = 0, major_bed = FALSE) {
  reach_length <- as.numeric(dist(node_coords[1:2,]))
  nb_sections <- ceiling(reach_length / space_step) + 1
  section_centers <- get_section_centers(node_coords, nb_sections)
  abscissas <- start + sapply(seq_len(nrow(section_centers)), function(i) {as.numeric(dist(section_centers[c(1,i), ]))})
  reach <- dem_to_reach(dem, node_coords, section_centers, section_width, nb_points)
  reach_txt <- lapply(seq_along(reach), function(i) {
    section_name <- paste0(ifelse(major_bed, "Major", "Minor"), " x=", abscissas[i])
    profile <- reach[[i]]
    if (major_bed != FALSE) {
      distance_majeur <- abscissas[i] - abscissas[max(i - 1, 1)]
    } else {
      distance_majeur <- FALSE
    }
    create_section_txt(section_name, abscissas[i], "A", profile, distance_majeur)
  })
  names(reach_txt) <- sprintf("%08d", abscissas)
  class(reach_txt) <- c("ReachTxt", class(reach_txt))
  return(reach_txt)
}


#' @rdname dem_to_reach
#' @param section_centers See return value of [get_section_centers]
#' @export
dem_to_reach <- function(dem, node_coords, section_centers, section_width, nb_points = 50) {
  if (nrow(node_coords) != 2 || ncol(node_coords) != 2)
    stop("`node_coords` should be a matrix of dimensions 2x2")
  lapply(seq_len(nrow(section_centers)), function(i) {
    section_center <- section_centers[i,]
    dem_to_section(dem, node_coords, section_center, section_width, nb_points)
  })
}


#' Create a section cross profile from a DEM
#'
#' @inheritParams dem_to_reach
#' @param section_center 2-lenght [numeric], coordinates of the section center
#'
#' @return A [matrix] with the coordinates of the x-z points in the cross-profile section referential
#' @export
#'
#' @inherit dem_to_reach return examples
dem_to_section <- function(dem, node_coords, section_center, section_width, nb_points = 50) {
  if (length(section_center) != 2) {
    stop("`section_center` should be of lenght 2")
  }
  section_bounds <-
    t(sapply(c(-1, 1), function(direction) {
      get_section_bound(node_coords, section_center, section_width, direction)
    }))
  section_points <-
    apply(section_bounds, 2, function(x) {
      seq(x[1], x[2], length.out = nb_points)
    })
  z <- terra::extract(dem, section_points, method = "bilinear")$lyr.1
  x_points <- sapply(seq_len(nrow(section_points)), function(i) {as.numeric(dist(section_points[c(1,i), ]))})
  m <- matrix(c(x_points, z), ncol = 2)
  colnames(m) <- c("x", "z")
  return(m)
}

get_section_bound <- function(node_coords, section_center, section_width, direction = 1) {
  theta_reach <- atan2(diff(node_coords[,2]), diff(node_coords[,1]))
  theta <- theta_reach + direction * pi / 2
  r <- section_width / 2
  c(x = section_center[1] + r * cos(theta), y = section_center[2] + r * sin(theta))
}

#' Get coordinates of section centers
#'
#' From the coordinates of a segment, this function returns the coordinates
#' of equidistant points along the segment given a number of points
#'
#' @param node_coords 2-rows [matrix], with coordinates of segment boundaries
#' @param nb_sections 1-length [numeric], number of points to extract
#'
#' @return A [matrix] with the coordinates of the points along the segment
#' @export
#'
#' @examples
#' node_coords <- matrix(c(102550, 102550, 110000, 108000), ncol = 2)
#' reach_length <- as.numeric(dist(node_coords[1:2,]))
#' space_step <- 100
#' nb_sections <- ceiling(reach_length / space_step) + 1
#' get_section_centers(node_coords, nb_sections)
get_section_centers <- function(node_coords, nb_sections) {
  apply(node_coords, 2, function(x) {
    seq(x[1], x[2], length.out = nb_sections)
  })
}
