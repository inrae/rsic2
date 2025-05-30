% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_result.R
\name{read_bin_result_matrix}
\alias{read_bin_result_matrix}
\title{Read matrix of SIC simulation result file}
\usage{
read_bin_result_matrix(cfg, scenario, variant = 0)
}
\arguments{
\item{cfg}{a \link{config} object. Configuration to use. See \link{loadConfig} for details}

\item{scenario}{\link{numeric}, the scenario to use}

\item{variant}{\link{numeric}, the variant to use (0 by default means no variant)}
}
\value{
\link{matrix} with the simulation results
}
\description{
Read matrix of SIC simulation result file
}
\examples{
\dontrun{
cfg <- cfg_tmp_project()
sic_run_steady(cfg, scenario = 1)
m <- read_bin_result_matrix(cfg, 1)
str(m)
}
}
