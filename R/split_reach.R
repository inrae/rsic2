#' Split a reach into a list of reaches
#'
#' @param reach A ReachTxt object
#' @param x_limits Chainage (i.e. abscissas along the reach) of splitting points
#'
#' @return A [list] of ReachTxt objects
#' @export
#'
#' @examples
split_reach <- function(reach, x_limits) {
  if(length(x_limits) < 2) {
    stop("`x_limits` length must be greater or equal to 2")
  }
  lapply(seq_len(length(x_limits) - 1), function(i) extract_reach(reach, x_limits[i:(i+1)]))
}

#' Select portion of a reach between two chainages
#'
#' @param reach A ReachTxt object
#' @param x_limits 2-length [numeric], min and max chainage
#'
#' @return A ReachTxt object containing the selected sections.
#' @export
#'
#' @examples
extract_reach <- function(reach, x_limits) {
  reach_names <- names(reach)
  x_limits <- sprintf("%08d", x_limits)
  sel_reach <- reach[reach_names >= min(x_limits) & reach_names <= max(x_limits)]
  class(sel_reach) <- c("ReachTxt", class(sel_reach))
  return(sel_reach)
}
