% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sensor_and_truth_data_plotly.R
\name{plot_sensor_and_truth_data_plotly}
\alias{plot_sensor_and_truth_data_plotly}
\title{Plot both sensor and truth data with plotly}
\usage{
plot_sensor_and_truth_data_plotly(
  scenario,
  useDefaultColors = FALSE,
  legendOnly = FALSE
)
}
\arguments{
\item{scenario}{MUST contain one of: targetTruth, ownShipTruth, and sensorData}

\item{useDefaultColors}{(default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true}

\item{legendOnly}{(boolean, default = FALSE) Initially populates plot with only the legend.  Data can be turned on by selecting the data from the legend.  Good for plots with a large number of traces}
}
\value{
plotly object
}
\description{
This figure shows an overview of the test and plots the ownship data (thin black line) along with the truth data (solid lines) and the tracks from the sensor (points with connecting lines).  Targets and tracks are not labeled because they can be seen by hovering over a point in plotly figures.  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.

This figure can be created before any tracks are assigned to targets to ensure that the user has imported the data correctly and to get an overall picture of the test.
}
\examples{
plot_sensor_and_truth_data_plotly(example1_scenario)
}
