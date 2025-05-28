#' Split a reach into a list of reaches
#'
#' @param reach A ReachTxt object
#' @param x_limits Chainage (i.e. abscissas along the reach) of splitting points
#'
#' @return A [list] of ReachTxt objects
#' @export
#'
#' @examples
#' # Create a 10km long trapezoidal uniform reach
#' profT <- list(
#'   B = 2,
#'   S = (6 - 2) / 2 / 2,
#'   ZF = 100,
#'   ZB = 100 + 2
#' )
#' min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
#'                                       upstream_bed_elevation = 10 + 2000 * 0.002,
#'                                       slope = 0.002,
#'                                       section_type = "T",
#'                                       profile = profT)
#' # Split into reaches of 2000 m
#' reaches <- split_reach(min_reach, seq(0, 10000, 2000))
#'
split_reach <- function(reach, x_limits) {
  if (length(x_limits) < 2) {
    stop("`x_limits` length must be greater or equal to 2")
  }
  lapply(
    seq_len(length(x_limits) - 1),
    function(i) extract_reach(reach, x_limits[i:(i + 1)])
  )
}

#' Select portion of a reach between two chainages
#'
#' This function is used by [split_reach] for selecting sections of each reach.
#'
#' @param reach A ReachTxt object
#' @param x_limits 2-length [numeric], min and max chainage
#'
#' @return A ReachTxt object containing the selected sections.
#' @export
#'
#' @examples
#' # Create a 10km long trapezoidal uniform reach
#' profT <- list(
#'   B = 2,
#'   S = (6 - 2) / 2 / 2,
#'   ZF = 100,
#'   ZB = 100 + 2
#' )
#' min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
#'                                       upstream_bed_elevation = 10 + 2000 * 0.002,
#'                                       slope = 0.002,
#'                                       section_type = "T",
#'                                       profile = profT)
#' # Extract sections between chainage 2000 m and 4000 m
#' sel_reach <- extract_reach(min_reach, c(2000, 4000))
#'
extract_reach <- function(reach, x_limits) {
  reach_names <- names(reach)
  x_limits <- sprintf("%08d", x_limits)
  sel_reach <- reach[
    reach_names >= min(x_limits) & reach_names <= max(x_limits)
  ]
  # Change major bed distance of the last section (issue #13)
  i_last <- length(sel_reach)
  s_cfg <- strsplit(sel_reach[[i_last]][1], "$", fixed = TRUE)[[1]]
  if (s_cfg[4] == " 1 ") {
    s_cfg[3] = " 0 "
    sel_reach[[i_last]][1] <- paste(s_cfg, collapse = "$")
  }

  class(sel_reach) <- c("ReachTxt", class(sel_reach))
  return(sel_reach)
}
