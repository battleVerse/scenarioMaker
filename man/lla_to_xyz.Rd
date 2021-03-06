% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lla_to_xyz.R
\name{lla_to_xyz}
\alias{lla_to_xyz}
\title{Convert Earth lon/lat/alt to cartesian coordinates}
\usage{
lla_to_xyz(lon, lat, alt)
}
\arguments{
\item{lon}{Longitude in degrees}

\item{lat}{Latitude in degrees}

\item{alt}{Altitude in meters}
}
\value{
A list of 3 numbers where the 3 entries correspond to x, y, and z cartesian coordinates
}
\description{
This function converts Earth latitude/longitude/altitude to cartesian coordinates (for interpolation functions). Cartesian coordinates are from the center of Earth.  Meant to be used in conjunction with \code{\link{xyz_to_lla}}
}
\examples{
scenarioMaker:::lla_to_xyz(120, 42, 1005)

}
\keyword{internal}
