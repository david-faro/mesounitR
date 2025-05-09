% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_empirical_mesosize.R
\name{compute_empirical_mesosize}
\alias{compute_empirical_mesosize}
\title{Compute empirical mesoscale size parameters}
\usage{
compute_empirical_mesosize(hmu_map, W)
}
\arguments{
\item{hmu_map}{A \code{sf} object representing surveyed mesohabitats.}

\item{W}{A numeric scalar representing the average wetted channel width}
}
\value{
A data frame with two columns:
\describe{
\item{c_logmean}{The mean of the log-transformed normalized meso sizes.}
\item{c_logsd}{The standard deviation of the log-transformed normalized meso sizes.}
}
}
\description{
This function computes the normalized meso sizes from a spatial object (mesohabitat survey)
and returns the log-normal parameters (mean and standard deviation) of their distribution.
}
\examples{
\dontrun{
library(sf)
# Assuming `hmu_map` is an sf object and domain width is known
compute_empirical_mesosize(hmu_map, W = 1000)
}

}
