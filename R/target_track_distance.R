#' @title Target to sensor track distance
#'
#' @description
#'
#' NOTE: This function is called automatically by create_scenario().  If you're using this, you likely want create_scenario() as it runs not only this function but also creates a scenario that can be used in most other functions in this package.  If you're thinking of using this, check out create_scenario() first and make sure that you really want this function.
#'
#' This function calculates the ranges, bearings, target aspects, and errors associated with every possible target-track pair. It is intended to be used with target_assignment() which will then remove most of these and keep exactly one target per sensor point. This function works as follows:
#'
#' First, the input is checked using check_input() to make sure that the input data frames are in the correct format. See check_input() for details on this as well as the Input section above for the correct format.
#'
#' Next, NAUTILUS calculates the disance between each target-track pair. It does this by filtering the truth data to separate each target, calculates all of the desired parameters for the target compared to every sensor track, then repeats this for every target. Ultimately these are combined to one data frame for the output.
#'
#' It is assumed that the truth data and ownship position will have a measurement frequency equal to or greater than the sensor system. Therefore, to calculate the target-track distance we interpolate the target positions and ownship's position to the time of each sensor point. The lat, lon, and alt positions are interpolated on a spheroidal Earth (WGS84) using the function interp_ellipse().
#'
#' During this step, Nautilus also interpolates the target and ownship heading to the sensor points. It converts the angle to sin and cos, interpolates those, and then recalculates the angles. It does this to avoid situations when the heading (in azimuth) changes rapidly from 359 degrees back to 0 degrees.
#'
#' After interpolation, Nautilus then calculates all of the desired output results such as the range between targets and the sensor system and the error (see Output format section above). The range between ownship and the targets is based on the truth data at the time of the sensor point, as are the target bearing and target aspect. It also calculates the difference (error) between each sensor track and each target. Many of these will be removed after the track is assigned to exactly one target in target_assignemnt(), however they are kept in this output.
#'
#' The distances (ranges) between tracks and targets or targets and ownship is the straight-line distance calculated by converting lon/lat/alt into xyz then taking the square root of the sum of squares. We have written functions that convert lon/lat/alt to xyz and back called lla_to_xyz() and xyz_to_lla().
#'
#' After the output values for each target-track pair are calculated they are placed into one data frame and returned to the user.
#'
#' This function does a large number of calculations and is intended to be used in conjunction with target_assignemnt(). All of the target-track combinations are calculated, but in target_assignment() every point of each track will be assigned to a single target. The output of this function can be piped into target_assignment(). It is not recommended that the user use this function as a stand alone function, but rather to always follow it with target_assignment() which assigns each track to a single target.
#'
#' @param truthData data frame containing all of the truth data for each target (likely from GPS or land-based radar systems). MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) target heading in degrees azimuth}
#'}
#'
#' @param sensorData data frame containing each sensor point for all of the tracks. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{trackNum: (factor) identifier for the track. We recommend numbers for each unique track returned by the sensor system}
#'  }
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
#'
#' @return A dataframe containing all distances, bearings, etc. for every track/target pair. For example, if there were four targets (A through D) and five tracks (1 though 5), then the output contains the distance between target and track for all 20 combinations. The output columns are very similar to that of target_assignment(), however target_assignment() keeps only the target-track pairs that were assigned.
#'
#' It includes:
#' \itemize{
#'   \item{lonError: difference in longitude between the sensor point and target}
#'   \item{latError: difference in latitude between the sensor point and target}
#'   \item{altError: difference in altitude between the sensor point and target}
#'   \item{relBearingToTarget: relative bearing (azimuth) to target from ownship}
#'   \item{trueBearingToTarget: true bearing (azimuth) to target from ownship}
#'   \item{trackNum: the track number associated with this target-track pair}
#'   \item{truthID: the target ID associated with this target-track pair}
#'   \item{locationError: distance between sensor point and target at the given time}
#'   \item{pointIndex: index referring to which of the original sensor data points this target-track pair refers. Necessary for comparisons in target_assignment()}
#'   \item{time: the time that this sensor point was recorded}
#'   \item{bearingError: difference in calculated bearing between the sensor point and target}
#'   \item{downrangeError: difference in range to ownship between the sensor point and target}
#'   \item{lon: longitude of the sensor point}
#'   \item{lat: latitude of the sensor point}
#'   \item{alt: altitude of the sensor point}
#'   \item{rangeToShip: range from target to ownship at the time of the sensor data point}
#'   \item{targetAspect: target aspect (as seen from ownship) at the time of the sensor data point}
#'   }
#'
#' @export



# function that takes truth and radar data and returns the distances from all targets to all tracks
# using Benjamin's accuracy algorithm

target_track_distance <- function(truthData, sensorData, ownShipData){
    #user facing

    ################################################################################################
    ### Description: Takes truth data from targets, truth data from ownship, and sensor data
    ### and compares the where the sensor THOUGHT the targets were to where they ACTUALLY were,
    ### and calculates many values of interest along the way (e.g., distance between sensor and
    ### target at time of measurement)
    ###
    ### Input: truthData, sensorData, and ownshipData all have the following:
    ### 'time' (must be number that monotonically increases), 'lat' (degrees), 'lon' (degrees),
    ### 'alt' (height ASL in meters), and 'heading' (heading in degrees, 0 = north, clockwise).
    ### In addition, truthData has 'truthID', a factor with a level for each target,
    ### sensorData has 'trackNum', a factor with a level for each sensor track. Ownship
    ### has no comparable column.
    ###
    ###
    ### Output: Returns a new dataframe comparing where the sensor SAID each target was to
    ### where ALL the targets REALLY were. Each target will compare itself against all radar tracks
    ### The results from this function should be passed to target_assignment to figure out which
    ### track belongs with which target.
    ###
    ### Note: the 'pointIndex' value is very important. Index 1 is the first sensor point in
    ### the sensor file (presumably but not necessarily the first in time) compared to each
    ### truth track (so with 4 truth targets, there will be 4 rows with pointIndex=1), and
    ### index 2 is the second sensor point compared to each target. So the all the pointIndex=1
    ### can be compared apples to apples in the next function to determine which target
    ### we should associate with the track
    ################################################################################################

    ### Check the input dataframes to make sure they're okay ###
    verify_input_data(myData=sensorData,columnNames=c("time","lat","lon","trackNum","alt"),factorNames="trackNum", stopOnFail=TRUE)
    verify_input_data(myData=truthData,columnNames=c("time","lat","lon","truthID","heading","alt"),factorNames="truthID", stopOnFail=TRUE)
    verify_input_data(myData=ownShipData,columnNames=c("time","lat","lon","truthID","heading","alt"),factorNames="truthID", stopOnFail=TRUE)


    # we're going to build this list as we go through the loop
    # each entry will be one truth target compared to all sensor tracks
    tempList=list()
    i=1

    #this bit just gets the error for every sensor point agianst every truth position - this is the 'dumb' part of the code, the clever part comes later
    for (target in (unique(truthData$truthID))) { #go through each target

        thisTargetTruth=filter(truthData,truthID==target) #pull out data for this particular target

        ##############################################################################################
        ### Interpolate the truth data of the targets to match the time of the sensor measurements ###
        ##############################################################################################



        # interpolate lat, lon, alt using WGS84.
        interpolatedEllipse <- interp_ellipse(thisTargetTruth, unlist(sensorData$time))

        interpolatedTargetLon <- interpolatedEllipse$lon
        lonError <- interpolatedTargetLon-sensorData$lon

        interpolatedTargetLat <- interpolatedEllipse$lat
        latError <- interpolatedTargetLat-sensorData$lat

        interpolatedTargetAlt <- interpolatedEllipse$alt
        altError <- interpolatedTargetAlt-sensorData$alt


        #interpolate the truth target's heading to the sensor times.
        interpolatedTargetHeading=interp_heading(thisTargetTruth$time,thisTargetTruth$heading,sensorData$time)


        ### Now interpolate ownship position to sensor times ###

        # interpolate lat, lon, alt using WGS84.
        interpolatOwnship <- interp_ellipse(ownShipData, unlist(sensorData$time))

        interpOwnShipLon <- interpolatOwnship$lon
        interpOwnShipLat <- interpolatOwnship$lat
        interpOwnShipAlt <- interpolatOwnship$alt



        #interpolate own ship's heading to the sensor times.
        interpOwnShipHeading=interp_heading(ownShipData$time,ownShipData$heading,sensorData$time)



        ############################################################################################################
        ### Now that we know where everything was on the same timestamps, let's start calculating the difference ###
        ### between what the sensor reported and reality                                                         ###
        ############################################################################################################


        # figure out the distance (in meters) between where the sensor THOUGHT the target was and where it REALLY was
        targetXYZ <- lla_to_xyz(interpolatedTargetLon, interpolatedTargetLat, interpolatedTargetAlt)
        ownShipXYZ <- lla_to_xyz(interpOwnShipLon, interpOwnShipLat, interpOwnShipAlt)
        sensorXYZ <- lla_to_xyz(sensorData$lon, sensorData$lat, sensorData$alt)

        locationError <- sqrt( (targetXYZ[[1]] - sensorXYZ[[1]])^2 +
                                   (targetXYZ[[2]] - sensorXYZ[[2]])^2 +
                                   (targetXYZ[[3]] - sensorXYZ[[3]])^2 )

        # range to ship is based on truth data and own ship data
        rangeToShip <- sqrt( (targetXYZ[[1]] - ownShipXYZ[[1]])^2 +
                                 (targetXYZ[[2]] - ownShipXYZ[[2]])^2 +
                                 (targetXYZ[[3]] - ownShipXYZ[[3]])^2 )

        # Calculate downrange error (range sensor to ship - range truth to ship)

        rangeToSensor <- sqrt( (sensorXYZ[[1]] - ownShipXYZ[[1]])^2 +
                                   (sensorXYZ[[2]] - ownShipXYZ[[2]])^2 +
                                   (sensorXYZ[[3]] - ownShipXYZ[[3]])^2 )

        downrangeError <- rangeToSensor - rangeToShip



        # calculating (true) relative bearing of the target from the sensor's perspective (i.e., bearing - ship heading)
        trueBearingToTarget <- geosphere::bearing(cbind(interpOwnShipLon, interpOwnShipLat),
                                                  cbind(interpolatedTargetLon, interpolatedTargetLat)) %>%
            unlist()
        trueBearingToTarget <- trueBearingToTarget %% 360 # convert from -180/180 range to 0/360 range
        relBearingToTarget <- (trueBearingToTarget - interpOwnShipHeading) %% 360

        # get the difference in bearing  from ownship to target between what the sensor reported and reality (bearing error)
        # (truth bearing - sensor bearing)
        # note: if ownShip is pointing right at the ship in question, the sensor and truth can be off by ~360 degrees.  For example, if true bearing = 359 and sensor bearing = 1, bearingError will give -358.  BUT I don't want to just do bearingError%%360 here because it will give only positive bearing errors.  Hence the last two 'ifelse' lines below
        radarBearing <- geosphere::bearing(cbind(interpOwnShipLon, interpOwnShipLat),
                                           cbind(sensorData$lon, sensorData$lat)) %>%
            unlist()

        bearingError <- ((radarBearing - interpOwnShipHeading) %% 360) - relBearingToTarget
        bearingError <- ifelse(bearingError >  180, -360 + bearingError, bearingError)
        bearingError <- ifelse(bearingError < -180,  360 + bearingError, bearingError)


        # get target aspect (this is the angle you are seeing the target - i.e. are you seeing its bow, beam, or stern)
        # heading - reverse bearing (target to ownship)
        targetAspect <- lapply(1:length(interpolatedTargetLon),
                               function(x) geosphere::bearing(c(interpolatedTargetLon[x], interpolatedTargetLat[x]),
                                                              c(interpOwnShipLon[x], interpOwnShipLat[x]))) %>%
            unlist()
        targetAspect <- ( targetAspect - interpolatedTargetHeading ) %% 360


        #add the data as a new item in a list (see header comments for explanation of pointIndex!)
        tempList[[i]]=data.frame(lonError,
                                 latError,
                                 altError,
                                 relBearingToTarget,
                                 trueBearingToTarget,
                                 trackNum=as.factor(sensorData$trackNum),
                                 truthID=as.factor(target),
                                 locationError=locationError,
                                 pointIndex=1:length(lonError),
                                 time = sensorData$time,
                                 bearingError,
                                 downrangeError,
                                 lon=sensorData$lon,
                                 lat=sensorData$lat,
                                 alt=sensorData$alt,
                                 rangeToShip,
                                 targetAspect)

        i=i+1
    }

    #combine the different entries
    targetTrackDistance=do.call(rbind,tempList)

    return(targetTrackDistance)
}





