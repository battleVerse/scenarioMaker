#' @title Track to ownship range
#'
#' @description
#'
#' NOTE: This function is called automatically by create_scenario().  If you're using this, you likely want create_scenario() as it runs not only this function but also creates a scenario that can be used in most other functions in this package.  If you're thinking of using this, check out create_scenario() first and make sure that you really want this function.
#'
#' This function is a simplified version of target_track_distance() (and the closely-related target_ownship_distance()) that calculates only the ranges and bearings of each sensor track during the test. This function may be important when inspecting where the sensor system believed that the targets were and when initially inspecting data.  For example, some data that you receive may show the sensor system's reference frame during operation, and this function will return the results of where the sensor system believed that there were targets.
#'
#' This functions works similarly to target_track_distance() and target_ownship_distance(). The lon, lat, and alt of ownShip is interpolated to the time of the sensor system measurements using a spheroidal Earth (WGS84 projection). Nautilus uses the function interp_ellipse() to interpolate these positions. To avoid discontinuities (i.e., the jump from 359 degrees to 0), target headings are interpolated by first converting to sin and cos.
#'
#' The target slant ranges are calculated using the straight-line distance by converting lon/lat/alt of the target and ownship to cartesian coordinates and then taking the square root of the sum of the squares.  The ground ranges are claculated using the WGS84 projection.
#'
#' This function can be used as a stand alone function or in conjunction with analysis functions to get the truth ranges and bearings of each target during the test.

#' @param sensorData data frame containing each sensor point for all of the tracks. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{trackNum: (factor) identifier for the track. We recommend numbers for each unique track returned by the sensor system}
#'  }
#'
#'
#' @param ownShipData data frame containing all of the truth position of the sensor system (likely from GPS or land-based radar systems). This may be ownship if testing something on a ship or the lat/lon position of a stationary ststem. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) sensor system heading in degrees azimuth}
#'}
#'
#' @return A dataframe containing the ranges and bearings from the sensor system to every target at each time the position of the sensor system was measured. The output columns are a limited version of target_track_distance() and includes:
#' \itemize{
#'   \item{time: time of the measurement of ownship position}
#'   \item{ownShipTruthID: unique identifier for the reference platform}
#'   \item{ownShipLon: longitude of ownship at this time}
#'   \item{ownShipLat: latitude of ownship at this time}
#'   \item{ownShipAlt: altitude of ownship at this time}
#'   \item{slantRange: range from ownship to the track (straight-line distance)}
#'   \item{groundRange: range from ownship to the track (along surface of spheroidal Earth)}
#'   \item{trueBearingToTarget: true bearing (azimuth) to the target at this time}
#'   \item{relBearingToTarget: relative bearing (azimuth) to the target at this time}
#'   \item{trackNum: unique identifier for the track}
#'   \item{trackLon: target longitude at this time}
#'   \item{trackLat: target latitude at this time}
#'   \item{trackAlt: target altitude at this time}
#'   }
#'
#' @export
#'


# function that takes truth and radar data and returns the distances from all targets to all tracks
# using Benjamin's accuracy algorithm

track_ownship_distance <- function(sensorData, ownShipData){
    #user facing

    ################################################################################################
    ### Description: Takes truth data from targets and truth data from ownship and calculates
    ### the target positions, ranges, and aspects at each time for which ownship data exists.
    ### This is helpful for some of the figures and analysis
    ###
    ###
    ### Input: truthData and ownshipData both have the following:
    ### 'time' (must be number that monotonically increases), 'lat' (degrees), 'lon' (degrees),
    ### 'alt' (height ASL in meters), and 'heading' (heading in degrees, 0 = north, clockwise).
    ### In addition, truthData and ownShip have 'truthID', a factor with a level for each target,
    ### although in ownShip this can all be set to 'ownShip'
    ###
    ### Output: Returns a new dataframe calculating the true target ranges and headings as a
    ### function of time for use in scenario reconstruction.  Calculates range for each time
    ### that there exists ownship data.
    ###
    ################################################################################################

    verify_input_data(myData=sensorData,columnNames=c("time","lat","lon","trackNum","alt"),factorNames="trackNum", stopOnFail=TRUE)
    verify_input_data(myData=ownShipData,columnNames=c("time","lat","lon","truthID","heading","alt"),factorNames="truthID", stopOnFail=TRUE)

    ownShipTruthID=as.factor(unique(ownShipData$truthID))


    ###############################################################################
    ### Interpolate the ownship positions to match times of tracks (sensorData) ###
    ###############################################################################

    # interpolate lat, lon, alt using WGS84.
    interpolatOwnship <- interp_ellipse(ownShipData, unlist(sensorData$time))

    interpOwnShipLon <- interpolatOwnship$lon
    interpOwnShipLat <- interpolatOwnship$lat
    interpOwnShipAlt <- interpolatOwnship$alt


    #interpolate ownship's heading to the sensor times.
    interpolatedOwnShipHeading=interp_heading(ownShipData$time,ownShipData$heading,sensorData$time)



    #############################################################################################
    ### Now that we know where everything was on the same timestamps, let's start calculating ###
    ###  the range to ownship                                                                 ###
    #############################################################################################


    # figure out the distance (in meters) between where the sensor THOUGHT the target was and where it REALLY was
    trackXYZ <- lla_to_xyz(sensorData$lon, sensorData$lat, sensorData$alt)
    ownShipXYZ <- lla_to_xyz(interpOwnShipLon, interpOwnShipLat, interpOwnShipAlt)

    # range to ship is based on truth data and own ship data
    slantRange <- sqrt( (trackXYZ[[1]] - ownShipXYZ[[1]])^2 +
                            (trackXYZ[[2]] - ownShipXYZ[[2]])^2 +
                            (trackXYZ[[3]] - ownShipXYZ[[3]])^2 )

    targetLL = cbind(sensorData$lon, sensorData$lat)
    ownShipLL = cbind(interpOwnShipLon, interpOwnShipLat)
    groundRange <- geosphere::distGeo(targetLL,ownShipLL)

    # calculating (true) relative bearing of the target from the sensor's perspective (i.e., bearing - ship heading)
    trueBearingToTrack <- geosphere::bearing(cbind(interpOwnShipLon, interpOwnShipLat),
                                             cbind(sensorData$lon, sensorData$lat)) %>%
        unlist()
    trueBearingToTrack <- trueBearingToTrack %% 360 # convert from -180/180 range to 0/360 range
    relBearingToTrack <- (trueBearingToTrack - interpolatedOwnShipHeading) %% 360



    #add the data as a new item in a list (see header comments for explanation of pointIndex!)
    trackOwnshipDistance=data.frame(time = sensorData$time,
                             ownShipTruthID = ownShipTruthID,
                             ownShipLon = interpOwnShipLon,
                             ownShipLat = interpOwnShipLat,
                             ownShipAlt = interpOwnShipAlt,
                             slantRange = slantRange,
                             groundRange = groundRange,
                             trueBearingToTrack,
                             relBearingToTrack,
                             trackNum = sensorData$trackNum,
                             trackLon = sensorData$lon,
                             trackLat = sensorData$lat,
                             trackAlt = sensorData$alt)

    return(trackOwnshipDistance)
}

