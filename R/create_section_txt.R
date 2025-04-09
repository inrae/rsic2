#' Export a section in importation SIC format
#'
#' @param section_name [character], name of the section
#' @param abscissa [numeric], abscissa of the section in the reach
#' @param section_type 1-length [character], type of the section: "T" for Trapezoidal, "A" for Abscissa/Elevation, "L" for Width/Elevation
#' @param profile [list] or [matrix], profile of the section (See details)
#' @param distance_majeur [logical] or [numeric], `FALSE` for a minor bed section
#' @param singular [logical] `TRUE` for a singular section, `FALSE` for a regular section
#'
#' @details
#' The parameter `profile` should respect the formats depending on section
#' types.
#' For parametrized section (rectangular, circular, trapezoidal), `profile`
#'
#' @return [character], section description in SIC text import format.
#' @export
#' @import utils
#' @examples
#' # Trapezoidal section
#' create_section_txt("Trapeze", 0, "T", list(B = 2, S = 1.5, ZF = 100, ZB = 102))
#' # Rectangular section
#' create_section_txt("Rectangle", 0, "R", list(B = 2, ZF = 100, ZB = 102))
#' # Circular section
#' create_section_txt("Circular", 0, "C", list(R = 0.5, ZF = 100, ZB = 102))
create_section_txt <- function(
  section_name,
  abscissa,
  section_type,
  profile,
  distance_majeur = FALSE,
  singular = FALSE
) {
  stopifnot(
    is.character(section_name),
    length(section_name) == 1,
    is.numeric(abscissa),
    length(abscissa) == 1,
    is.character(section_type),
    length(section_type) == 1
  )
  # Hot fix for SIC importation rectangular section is not handled
  if (section_type == "R") {
    check_section_type(section_type, profile, c("B"))
    section_type = "L"
    profile <- matrix(data = c(profile$B, profile$B, profile$ZF, profile$ZB), ncol = 2)
  } else if (section_type == "C") {
    check_section_type(section_type, profile, c("R"))
    section_type = "L"
    centre_z <- profile$ZF + profile$R
    all_z <- profile$ZF + seq(0, profile$R * 2, length.out = 22)
    widths <- sapply(all_z, function(z) {
        2 * sqrt(abs(profile$R^2 - (z - centre_z)^2))
    })
    profile <- round(cbind(widths, all_z), 4)
  }
  if (section_type %in% c("A", "L")) {
    if (!is.matrix(profile) || ncol(profile) != 2) {
      stop(
        "With `section_type = \"A\" or \"L\", `profile` should be a matrix with 2 columns"
      )
    }
    sic_profile <- capture.output(write.table(
      profile,
      sep = "\t",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    ))
  } else {
    # Parametrized sections
    if (section_type == "R") {
      check_section_type(section_type, profile, c("B"))
      sic_profile <- c("", profile$B)
    } else if (section_type == "C") {
      check_section_type(section_type, profile, c("R"))
      sic_profile <- c("", profile$R)
    } else if (section_type %in% c("T", "D")) {
      check_section_type(section_type, profile, c("B", "S"))
      sic_profile <- c(paste(profile$B, profile$S, sep = "\t"))
    } else {
      stop(
        "section_type ",
        section_type,
        " not allowed. Possible choices are: A, L and T"
      )
    }
    sic_profile <- c(sic_profile, paste(profile$ZB, profile$ZF, sep = "\t"))
  }

  if (
    !(is.logical(distance_majeur) || is.numeric(distance_majeur)) ||
      length(distance_majeur) != 1
  ) {
    stop("`distance_majeur` should be a `logical` or a `numeric` of length 1")
  }
  bMajorBed <- !is.logical(distance_majeur)
  section_txt <- c(
    paste(
      section_name,
      abscissa,
      ifelse(bMajorBed, distance_majeur, ""),
      ifelse(bMajorBed, "1", "0"),
      paste0(section_type, ifelse(singular, "S", "")),
      sep = " $ "
    ),
    sic_profile
  )
  class(section_txt) <- c("SectionTxt", class(section_txt))
  return(section_txt)
}

check_section_type <- function(section_type, profile, required_params) {
  required_params <- c(required_params, "ZF", "ZB")
  if (!is.list(profile) || !all(required_params %in% names(profile))) {
    stop(
      "With `section_type = \"",
      section_type,
      "\", `profile` should be a list with the items",
      paste(required_params, collapse = ", ")
    )
  }
}
