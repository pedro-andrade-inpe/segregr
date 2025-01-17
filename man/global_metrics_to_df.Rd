% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_metrics_to_df.R
\name{global_metrics_to_df}
\alias{global_metrics_to_df}
\title{Title}
\usage{
global_metrics_to_df(segregation_results, bandwidths = c())
}
\arguments{
\item{segregation_results}{Result of measure_segregation().}

\item{bandwidths}{Attributes to be used. Default is all.}
}
\value{
A data.frame.
}
\description{
Title
}
\examples{
marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))

segregation <- segregr::measure_segregation(marilia_sf)
df <- segregr::global_metrics_to_df(segregation)
}
