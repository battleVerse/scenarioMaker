#' @title Transform offset to lon/lat
#'
#' @description This function takes sensor data given in offset units (meters north / meters east of sensor / meters up from sensor location) and converts it to latitude and longitude based on the sensor system position.  Nautilus uses latitude and longitude, so units must be converted.
#'
#' @param referenceData data frame containing the longitude and latitude position of the sensor system.  MUST contain:
#' \itemize{
#'   \item{time: time of measurement (POSIX)}
#'   \item{lat: latitude at time of measurement (degrees)}
#'   \item{lon: longitude at time of measurement (degrees)}
#'   \item{alt: altitude at time of measurement (meters)}
#'   }

#' @param relativeSensorData data frame containing sensor readings of targets.  MUST contain:
#' \itemize{
#'   \item{time: time of measurement (POSIX)}
#'   \item{NorthOffset: meters North of sensor}
#'   \item{EastOffset: meters East of sensor}
#'   \item{UpOffset: meters above sensor}
#'   \item{trackNum: (factor) name of sensor track.  Number is recommended}
#'   }
#'
#' @return data frame with the lat and lon position of the targets.  Variables include:
#' \itemize{
#'   \item{time: time of measurement (POSIX)}
#'   \item{trackNum: (factor) name of sensor track.  Number is recommended}
#'   \item{lat: latitude (degrees)}
#'   \item{lon: longitude (degrees)}
#'   \item{alt: altitude (meters)}
#'   \item{any additional columns included in relativeSensorData (minus North/East/Up offset)}
#'  }
#'
#'
#' @export
#'
#'




transform_offset_to_latlon = function(referenceData,relativeSensorData){
        #user facing

        if ((min(referenceData$time) > min(relativeSensorData$time)) || (max(referenceData$time) < max(relativeSensorData$time))){
            s="The sensor data you're trying to interpolate begins before or ends after the reference data begins/ends. These overhanging sensor points will be dropped (there's nothing to interpolate them to!).\n"
            warning(s)
        }

        #this takes offsets in meters!
        shipLatAtSensor=stats::approx(referenceData$time,referenceData$lat,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point
        shipLonAtSensor=stats::approx(referenceData$time,referenceData$lon,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point
        shipAltAtSensor=stats::approx(referenceData$time,referenceData$alt,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point

        tmp=data.frame(geodesic_lat_long_offset(shipLatAtSensor,shipLonAtSensor,relativeSensorData$NorthOffset,relativeSensorData$EastOffset))

        updatedAlt=shipAltAtSensor+relativeSensorData$UpOffset

        sensorData=mutate(relativeSensorData,lat=tmp$latitude,lon=tmp$longitude, alt=updatedAlt) %>%
                select(-NorthOffset, -EastOffset, -UpOffset) %>%
            stats::na.omit()

        return(sensorData)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("NorthOffset", "EastOffset","UpOffset"))

#transform_bearing_range_to_latlon
#user facing
