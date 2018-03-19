#' @title Interpolate points on an ellipsoidal Earth (WGS84)
#'
#' @description The times when the sensor recorded a track are not necessarily the same times that truth data exists for each target. Therefore, to calculate the distance between tracks and targets at each time there was a sensor track we need to interpolate the target positions to those times.
#'
#' interp_ellipse() interpolates lat, lon, and altitude on a spheroidal Earth (WGS84). For times in interpTimes outside the time range in dataSet those times are coerced into the range. Therefore, there will never be extrapolation. This can lead to small effects before target data exists and after targets are destroyed, so the user should be careful.
#'
#' This function is called by target_track_distance() and target_ownship_distance(), however it can also be used on its own.
#'
#' @param dataSet data frame to be interpolated. Must contain lat, lon, alt, and time columns
#' @param interpTimes vector of times to interpolate to.
#' @param returnType (default = "lla") type of output. "lla" returns interpolated lat, lon, and alt values. "xyz" returns interpolated cartesian values (with the origin at the center of the Earth and z in the direction of the north pole).
#'
#' @return A data frame with the same number of observations as the length of interpTimes and columns of either lat/lon/alt on WGS84 Earth or x/y/z cartesian coordinates (with origin at the center of Earth), depending on the returnType parameter.
#'
#' @export
#'


### Functions to convert ellipse data to cartesian and back

interp_ellipse <- function(dataSet, interpTimes, returnType = 'lla'){
        # This function takes lat, lon, alt, converts to x,y,z, interpolates on a spline (with equivalent to approx(rule=2)), then converts back to lat, lon, alt.  Equations found at navipedia.net
        #
        # INPUTS:
        #  dataSet must have lat, lon, alt and time.
        #  interpTimes is a list of times that we want to interpolate to

        # a and f for WGS84
        a <- 6378137  # semi-major axis
        b <- 6356752.314245 # semi-major axis
        f <-  1/298.257223563  # flattening
        e2 <- 2*f - f^2  # Square of the eccentricity

        # calculate x,y,z for each point
        dataSet <- dataSet %>%
                ungroup() %>%
                mutate(N = a / sqrt(1- e2*(sin(lat*pi/180))^2) ,
                       x = (N + alt) * cos(lat*pi/180) * cos(lon*pi/180) ,
                       y = (N + alt) * cos(lat*pi/180) * sin(lon*pi/180),
                       z = (N * (1-e2) + alt) * sin(lat*pi/180))


        # coerce times outside range into the time range
        interpTimesCoerced <- ifelse(interpTimes < min(dataSet$time), min(dataSet$time), interpTimes)
        interpTimesCoerced <- ifelse(interpTimesCoerced > max(dataSet$time),
                                     max(dataSet$time), interpTimesCoerced)


        # interpolate x,y, and z.  NOTE: We are interpolating to the coerced times, but passing the actual requested times.  This way if we request a time before we have truth data it will interpolate it to the time when we have data, but return the requested time back to the user
        interpData <- data.frame(time = interpTimes,
                                 xInterp = spline(dataSet$time, dataSet$x, xout = interpTimesCoerced)$y,
                                 yInterp = spline(dataSet$time, dataSet$y, xout = interpTimesCoerced)$y,
                                 zInterp = spline(dataSet$time, dataSet$z, xout = interpTimesCoerced)$y)


        # convert xyz back to lat, lon, alt
        # taken from MATLAB Geodetic Toolbox (geodetic299, xyz2ell2)
        # Uses direct algorithm in B.R. Bowring, "The accuracy of geodetic latitude and height equations", Survey Review, v28 #218, October 1985, pp.202-206.
        interpData <- interpData %>%
                mutate(lon = atan2(yInterp,xInterp)*180/pi,
                       e = e2*(a/b)^2,
                       p = sqrt(xInterp^2 + yInterp^2),
                       r = sqrt(p^2 + zInterp^2),
                       u = atan(b * zInterp * (1+e * b / r)/(a * p)),
                       lat = atan((zInterp + e * b * sin(u)^3) / (p - e2*a*cos(u)^3))*180/pi,
                       v = a / sqrt(1-e2*sin(lat*pi/180)^2),
                       alt = p*cos(lat*pi/180) + zInterp * sin(lat*pi/180)- a^2/v)

        if(returnType == 'xyz') {
                interpData <- interpData %>%
                        select(time, x = xInterp, y = yInterp, z = zInterp)
        } else {
                interpData <- interpData %>%
                        select(time, lon, lat, alt)
        }

        return(interpData)

}
