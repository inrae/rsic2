#' Export a section in importation SIC format
#'
#' @param name [character], name of the section
#' @param abscissa [numeric], abscissa of the section in the reach
#' @param type 1-length [character], type of the section: "T" for Trapezoidal, "A" for Abscissa/Elevation, "L" for Width/Elevation
#' @param profile [list] or [matrix], profile of the section (See details)
#' @param distance_majeur [logical] or [numeric], `FALSE` for a minor bed section
#' @param as_list [logical], if `TRUE`, the returns value is a list with one section by item with the name of the section as key
#'
#' @return [character] or [list] depending on the `as_list` parameter.
#' @export
#' @examples
#' # Trapezoidal section
#' export_section_txt("Trapeze", 0, "T", list(B = 2, S = 1.5, ZF = 100, ZB = 102))
create_section_txt <- function(name, abscissa, type, profile, distance_majeur = FALSE) {

  if (type == "T") {
    if (!is.list(profile) || !all(c("B", "S", "ZF", "ZB") %in% names(profile))) {
      stop("With `type = \"T\", `profile` should be a list with the items B, S, ZF and ZB")
    }
    sic_profile <- c(paste(profile$B, profile$S, sep = "\t"), paste(profile$ZB, profile$ZF, sep = "\t"))
  } else if (type %in% c("A", "L")) {
    if (!is.matrix(profile) || ncol(profile) != 2) {
      stop("With `type = \"A\" or \"L\", `profile` should be a matrix with 2 columns")
    }
    sic_profile <- capture.output(write.table(profile, sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE))
  } else {
    stop("type ", type, " not allowed. Possible choices are: A, L and T")
  }

  if (!(is.logical(distance_majeur) || is.numeric(distance_majeur)) || length(distance_majeur) != 1) {
    stop("`distance_majeur` should be a `logical` or a `numeric` of length 1")
  }
  bMajorBed <- !is.logical(distance_majeur)
  c(paste(name,
          abscissa,
          ifelse(bMajorBed, distance_majeur, ""),
          ifelse(bMajorBed, "1", "0"),
          type,
          sep = " $ "),
    sic_profile)
}
