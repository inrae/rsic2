% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sic_inputs.R
\name{SicLocation}
\alias{SicLocation}
\title{Set a location of a SIC model input}
\usage{
SicLocation(location)
}
\arguments{
\item{location}{a \link{list} containing the location keys (see details)}
}
\value{
a \emph{SicLocation} object which is a \link{character} string in the same format as the locations described in the sic documentation of PAR files: \url{https://sic.g-eau.fr/Format-of-the-par-file}.
}
\description{
Set a location of a SIC model input
}
\examples{
# How to impose 5 m3/s in the upstream offtake of the model?
# Define location of the boundary condition to set
locations <- SicLocations(list(Nd = 1, Pr = 1, car = "Q"))
# Define its value
sicInputs <- SicInput(5, locations = locations)

# How to impose an hydrograph at the upstream offtake of the model?
sicInputs <- SicInput(data.frame(t = c(0, 3600, 7200), # time in seconds
                                v = c(5, 20, 5)),     # flows
                      locations = locations)
}
\seealso{
Other SicInput: 
\code{\link{SicInput}()},
\code{\link{SicLocations}()}
}
\concept{SicInput}
