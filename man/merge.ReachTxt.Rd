% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.ReachTxt.R
\name{merge.ReachTxt}
\alias{merge.ReachTxt}
\title{Merge several \emph{ReachTxt} objects into one}
\usage{
\method{merge}{ReachTxt}(x, y = NULL, ...)
}
\arguments{
\item{x}{First \emph{ReachTxt} object to merge}

\item{y}{Second \emph{ReachTxt} object to merge}

\item{...}{Other \emph{ReachTxt} objects to merge}
}
\value{
a \emph{ReachTxt} object (See \link{create_uniform_reach_txt} and \link{dem_to_reach}) containing the merged reaches.
}
\description{
Merge several \emph{ReachTxt} objects into one
}
\examples{
# Minor bed generation
profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)
min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
                         upstream_bed_elevation = 10 + 2000 * 0.002,
                         slope = 0.002,
                         section_type = "T",
                         profile = profT)

# Major bed generation
data("floodam_ead_dem")
dem <- terra::rast(floodam_ead_dem)
node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
space_step = 100
section_width = 5000
maj_reach <- dem_to_reach_txt(dem, node_coords, space_step, section_width, major_bed = TRUE)

# Merge reaches into one
reach <- merge(min_reach, maj_reach)
}
