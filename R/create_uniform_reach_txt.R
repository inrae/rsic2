#' Generate text export geometry for an uniform reach
#'
#' @param abscissas [numeric] vector of section abscissas
#' @param upstream_bed_elevation [numeric], upstream bed elevation (m)
#' @param slope [numeric], bed slope of the reach (m/m)
#' @param section_names [character] vector of section names
#' @inheritParams create_section_txt
#'
#' @return A [list] from which each item is a section exported by [create_section_txt].
#' Names of the list are the abscissas with trailing zeros for character sorting.
#' @export
#'
#' @examples
#' # Trapezoidal section
#' profT <- list(B = 2,S = 1, ZF = 100, ZB = 100 + 2)
#'
#' # Generate a reach with 2 sections at x=1000 and 2000
#' create_uniform_reach_txt(abscissas = c(1000, 2000),
#'                          upstream_bed_elevation = 100,
#'                          slope = 0.001,
#'                          section_type = "T",
#'                          profile = profT)
create_uniform_reach_txt <- function(abscissas,
                                     upstream_bed_elevation,
                                     slope,
                                     section_type,
                                     profile,
                                     section_names = paste0("Section x=", abscissas)) {

  sections <- lapply(seq_along(abscissas), function(i) {
    x <- abscissas[i]
    bed_elevation <- upstream_bed_elevation - (x - abscissas[1]) * slope
    shifted_prof <- shift_profile(section_type, profile, bed_elevation)
    create_section_txt(section_name = section_names[i],
                       abscissa = x,
                       section_type = section_type,
                       profile = shifted_prof)
  })
  names(sections) <- sprintf("%08d", abscissas)
  class(sections) <- c("ReachTxt", class(sections))
  return(sections)
}

shift_profile <- function(section_type, profile, bed_elevation) {
  shifted_prof <- profile
  if (section_type == "T") {
    shifted_prof$ZF <- bed_elevation
    shifted_prof$ZB <- profile$ZB + bed_elevation - profile$ZF
  } else if (section_type == "L") {
    shifted_prof[,2] <- shifted_prof[,2] + bed_elevation - min(profile[, 2])
  } else {
    stop("section_type ", section_type, " not allowed. Possible choices are: T and L")
  }
  return(shifted_prof)
}
