#' @title Transform sensor bias
#'
#' @description Sensor systems occasionally have a bias in bearing, range, or altitude, e.g., targets are always 2 degrees clockwise or 10 meters lower in altitude.  This function takes sensor data and ownship data given in lat/lon/alt units and transforms the bearing, range, or altitude AS SPECIFIED BY THE USER.  Note that this is not an automatic correction.  It returns the transformed sensor data in lat/lon/alt units.
#'
#' @param ownShipData data frame containing all of the truth position of the sensor system (likely from GPS or land-based radar systems). This may be ownship if testing something on a ship or the lat/lon position of a stationary ststem. MUST have the following columns (but can have more, for example heading and truthID):
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'}

#' @param sensorData data frame containing each sensor point for all of the tracks. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{trackNum: (factor) identifier for the track. We recommend numbers for each unique track returned by the sensor system}
#'  }
#'
#' @param biasBearing (Default = 0) amount to adjust bearing bias, in degrees azimuth.  Positive numbers are clockwise, negative are counterclockwise
#'
#' @param biasRange (Default = 0) amount to adjust range bias, in meters.  Positive numbers shift sensor points away from sensor system, negative numbers shift sensor points closer to sensor system
#'
#' @param biasAlt (Default = 0) amount to adjust altitude, in meters.  Positive numbers shift sensor point to higher altitude, negative numbers shift sensor points to lower altitude
#'
#' @return data frame with the lat and lon position of the targets.  Variables include:
#' \itemize{
#'   \item{time: time of measurement (POSIX)}
#'   \item{trackNum: (factor) name of sensor track.  Number is recommended}
#'   \item{lat: latitude (degrees)}
#'   \item{lon: longitude (degrees)}
#'   \item{alt: altitude (meters)}
#'  }
#'
#'
#' @export
#'
#' @examples
#'
#' ownShipData <- example1_ownShipData
#' sensorData <- example1_sensorData
#'
#' # Increasing the bearing of each sensor point by 10 degrees (i.e., clockwise)
#' adj1 <- transform_sensor_bias(ownShipData, sensorData, biasBearing = 10)
#'
#' # Increasing the range of each sensor point by 3000 meters
#' adj2 <- transform_sensor_bias(ownShipData, sensorData, biasRange = 3000)




transform_sensor_bias = function(ownShipData, sensorData, biasBearing = 0, biasRange = 0, biasAlt = 0){
    #user facing
    #takes lon/lat/alt of ownship and sensor points, converts to bearing and range, adjusts bias, then converts back to lon/lat/alt

    tempList=list()
    i = 1

    #this bit just gets the error for every sensor point agianst every truth position - this is the 'dumb' part of the code, the clever part comes later
    for (track in (unique(sensorData$trackNum))) { #go through each track

        thisTrackSensorData <- filter(sensorData, trackNum == track) #pull out data for this particular track

        ### Interpolate ownship position to match the sensor points
        # interpolate ownship lat, lon, alt using WGS84.
        interpolatedOwnShip <- interp_ellipse(ownShipData, unlist(thisTrackSensorData$time))


        ### Calculate bearing and range to sensor points from ownShip

        bearingToTarget <- geosphere::bearing(cbind(interpolatedOwnShip$lon, interpolatedOwnShip$lat),
                                   cbind(thisTrackSensorData$lon, thisTrackSensorData$lat))

        rangeToTarget <-geosphere:: distGeo(cbind(interpolatedOwnShip$lon, interpolatedOwnShip$lat),
                                   cbind(thisTrackSensorData$lon, thisTrackSensorData$lat))


        ### correct bias in bearing, range, and altitude

        adjustedBearing <- bearingToTarget + biasBearing

        adjustedRange <- rangeToTarget + biasRange

        adjustedAlt <- thisTrackSensorData$alt + biasAlt



        ### Get new lon/lat

        adjustedLonLat <- geosphere::destPoint(cbind(interpolatedOwnShip$lon, interpolatedOwnShip$lat),
                  adjustedBearing, adjustedRange) %>%
            as.data.frame()



        # put new values into adjustedSensorData

        tempList[[i]] <- data.frame(time = thisTrackSensorData$time, trackNum = as.factor(as.character(track)),
                   lon = adjustedLonLat$lon, lat = adjustedLonLat$lat, alt = adjustedAlt)
        i = i+1


    }

    adjustedSensorData <- do.call(rbind,tempList) %>%
        as.data.frame()

    return(adjustedSensorData)
}

