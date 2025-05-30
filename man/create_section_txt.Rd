% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_section_txt.R
\name{create_section_txt}
\alias{create_section_txt}
\title{Export a section in importation SIC format}
\usage{
create_section_txt(
  section_name,
  abscissa,
  section_type,
  profile,
  distance_majeur = FALSE,
  singular = FALSE,
  xgeo = NULL,
  ygeo = NULL
)
}
\arguments{
\item{section_name}{\link{character}, name of the section}

\item{abscissa}{\link{numeric}, abscissa of the section in the reach}

\item{section_type}{1-length \link{character}, type of the section: "T" for Trapezoidal, "A" for Abscissa/Elevation, "L" for Width/Elevation}

\item{profile}{\link{list} or \link{matrix}, profile of the section (See details)}

\item{distance_majeur}{\link{logical} or \link{numeric}, \code{FALSE} for a minor bed section}

\item{singular}{\link{logical} \code{TRUE} for a singular section, \code{FALSE} for a regular section}
}
\value{
\link{character}, section description in SIC text import format.
}
\description{
Export a section in importation SIC format
}
\details{
The parameter \code{profile} should respect the formats depending on section
types.
For parametrized section (rectangular, circular, trapezoidal), \code{profile}
}
\examples{
# Trapezoidal section
create_section_txt("Trapeze", 0, "T", list(B = 2, S = 1.5, ZF = 100, ZB = 102))
# Rectangular section
create_section_txt("Rectangle", 0, "R", list(B = 2, ZF = 100, ZB = 102))
# Circular section
create_section_txt("Circular", 0, "C", list(R = 0.5, ZF = 100, ZB = 102))
}
