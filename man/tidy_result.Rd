% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_result.R
\name{tidy_result}
\alias{tidy_result}
\alias{compact_tidy_result}
\title{Tidy a result simulation}
\usage{
tidy_result(res)

compact_tidy_result(res)
}
\arguments{
\item{res}{a \emph{SicResult} \link{matrix} provided by \link{get_result}}
}
\value{
\code{tidy_result} returns a \link{data.frame} with one line by variable and by saved simulation result time step :
\itemize{
\item one column per object type of the result (example: "bf" and "sn" for a section or "nd" and "pr" for an offtake)
\item one column "var" for the definition of the result variable
\item one column "t" for the simulation time of the result variable
\item one columne "value" for the value of the result variable
}

\code{compact_tidy_result} returns a \link{data.frame} with one line by variable :
\itemize{
\item one column per object type of the result (example: "bf" and "sn" for a section or "nd" and "pr" for an offtake)
\item one column "var" for the definition of the result variable
\item one column "values" containing a \link{data.frame}  with a column \code{value}
}

The data.frame contains an attribute "t" with the time of the saved simulation time steps in seconds.
}
\description{
Tidy a result simulation
}
\examples{
\dontrun{
cfg <- cfg_tmp_project()
# Run unsteady flow simulation (flood over one day)
sic_run_unsteady(cfg, iniParams = c(1, 0, 0, 1, 1))
# Get water elevation in reach #2, section #2
m <- get_result(cfg, 1, 1,
                  filters = c("bf=2", "sn=2", "var='Z'"))
res <- tidy_result(m)
plot(res$t, res$value)
# Formatting can be called directly through argument
# `fun_format` of `get_result` function
res <- get_result(cfg, 1, 1,
                  filters = c("bf=2", "sn=2", "var='Z'"),
                  fun_format = compact_tidy_result))
# Plot result in first object against simulation time
plot(attr(res, "t"), res$values[1][[1]])
}
}
