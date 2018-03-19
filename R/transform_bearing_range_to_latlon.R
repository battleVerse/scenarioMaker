#' @title Transform bearing/range to lon/lat
#'
#' @description This function takes sensor data given in bearing/range/alt units and converts it to latitude, longitude, altitude based on the sensor system position.  Nautilus uses latitude and longitude, so units must be converted. The function will correct for sensor altitude in a purely additive manner (i.e., if sensor is at 1,000 feet, and tgt is 2,000 feet above, then tgt altitude is 3,000 feet).
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
#'   \item{bearing: bearing to target (degrees azimuth)}
#'   \item{range: range to target (meters)}
#'   \item{alt: altitude of target (meters)}
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
#'  }
#'
#'
#' @export
#'
#'



transform_bearing_range_to_latlon = function(referenceData,relativeSensorData) {
        #user facing
        #takes range in meters, bearing in degrees
        #relativeSensorData must have time, tracknum, alt, as well as bearing and range

        shipLatAtSensor=approx(referenceData$time,referenceData$lat,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point
        shipLonAtSensor=approx(referenceData$time,referenceData$lon,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point
        shipAltAtSensor=approx(referenceData$time,referenceData$alt,xout=relativeSensorData$time)$y #this is where the ship is at the time of each sensor point

        positions=cbind(shipLonAtSensor,shipLatAtSensor)

        updatedAlt=shipAltAtSensor+relativeSensorData$UpOffset

        tmp=data.frame(geosphere::geodesic(positions,azi=relativeSensorData$bearing,d=relativeSensorData$range))

        sensorData=mutate(relativeSensorData,lat=tmp$latitude,lon=tmp$longitude, alt=updatedAlt) %>%
                select(time,trackNum,lat,lon,alt)

        return(sensorData)

}
#
# time=c(1,2,3,4)
# trackNum=c(1,1,1,1)
# alt=c(0,0,0,0)
# shipLat=c(21,20,19,18)
# shipLon=c(40,39,38,37)
# referenceData=data.frame(time=time,lat=shipLat,lon=shipLon)
#
# ###
# EastOffset=c(0,0,0,0)
# NorthOffset=c(10000,10000,10000,10000)
# relativeSensorData=data.frame(time=time, trackNum=trackNum,alt=alt,EastOffset=EastOffset,NorthOffset=NorthOffset)
# transform_offset_to_latlon(referenceData,relativeSensorData)
#
#
# ###
# bearing=c(90,90,90,90)
# range=c(10000,10000,10000,10000)
# relativeSensorData=data.frame(time=time, trackNum=trackNum,alt=alt,bearing=bearing,range=range)
# transform_bearing_range_to_latlon(referenceData,relativeSensorData)
