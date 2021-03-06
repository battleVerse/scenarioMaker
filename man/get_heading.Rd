% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_heading.R
\name{get_heading}
\alias{get_heading}
\title{Get heading based on lon and lat}
\usage{
get_heading(dataSet, truthIDColName = "truthID")
}
\arguments{
\item{dataSet}{data frame with lon, lat, time, and target/track identifiers}

\item{truthIDColName}{(default="truthID") (string) the name of the column with target/track identifiers}
}
\value{
dataSet with an additional column, heading, that is the average heading at each point.
}
\description{
Nautilus calculates errors as a function of bearing and target aspect and therefore requires heading (or bearing) for ownship and for each target. If the truth data already contains this information then this function is unnecessary. If not, then the user will be required to run get_heading() before inputting the data into target_track_distance().

get_heading() uses the bearing() function from the package geosphere. For each point, get_heading() calculates the bearing between the current point and the previous point as well as the current point and the following point. get_heading then averages these two numbers and returns the average as the bearing column of the output.
}
\examples{
get_heading(example1_scenario$targetTruth, "truthID")
}
