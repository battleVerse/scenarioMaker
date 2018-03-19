#' @title Interpolate heading
#'
#' @description Function to interpolate headings.  Heading interpolation can be a little tricky when you wrap around from 0 to 360, so this function takes care of that.
#'
#' Used by distance_between(), target_track_distance(), target_ownship_distance(), and track_ownship_distance().  It's likely that if you're looking at using this function you actually want one of those.
#'
#' @param currentTimes vector of the times of the existing heading measurements
#' @param currentHeadings vector of the existing heading measurements
#' @param newTimes vector of the times to be interpolated to

#' @return a vector of headings at the new times (newTimes)
#'
#' @export
#'

interp_heading = function(currentTimes, currentHeadings, newTimes) {
#interpolate the truth target's heading to the ownship times.  It spits back an error when trying to interpolate when the heading jumps from 0 to 360, so we convert to sin and cos, interpolate, then use atan2 to get the angle back.

    cosHeading = cos(currentHeadings * pi / 180)
    sinHeading = sin(currentHeadings * pi / 180)

    interpTgtCosHeading=approx(currentTimes,cosHeading,xout=unlist(newTimes),rule=2)$y
    interpTgtSinHeading=approx(currentTimes,sinHeading,xout=unlist(newTimes),rule=2)$y
    interpolatedTargetHeading <- (atan2(interpTgtSinHeading, interpTgtCosHeading) * 180/pi) %%360

    return(interpolatedTargetHeading)
}
