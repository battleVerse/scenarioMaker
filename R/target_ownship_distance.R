#' @title Target to ownship range
#'
#' @description
#'
#' NOTE: This function is called automatically by create_scenario().  If you're using this, you likely want create_scenario() as it runs not only this function but also creates a scenario that can be used in most other functions in this package.  If you're thinking of using this, check out create_scenario() first and make sure that you really want this function.
#'
#' This function is a simplified version of target_track_distance() that calculates only the truth ranges and bearings of each target during the test. It may be important when trying to compare the sensor results to the truth data. For example, if one target was never detected by the sensor system then the results of target_track_distance() and target_assignment() would give no information about how close it came to ownship during the test because those functions deal primarily with the output of the sensor system. target_ownship_distance() will calculate find this information which can be combined with other outputs to get a more complete picture of the test and how the sensor system compared to this.
#'
#' This functions works similarly to target_track_distance. The lon, lat, and alt of each target is interpolated to the time of ownship position measurements using a spheroidal Earth (WGS84). Nautilus uses the function interp_ellipse() to interpolate these positions. To avoid discontinuities (i.e., the jump from 359 degrees to 0), target headings are interpolated by first converting to sin and cos.
#'
#' The target ranges are calculated using the straight-line distance by converting lon/lat/alt of the target and ownship to cartesian coordinates and then taking the square root of the sum of the squares.
#'
#' This function can be used as a stand alone function or in conjunction with analysis functions to get the truth ranges and bearings of each target during the test.
#'
#' @param truthData truthData: data frame containing all of the truth data for each target (likely from GPS or land-based radar systems). MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) target heading in degrees azimuth}
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
#' @return A dataframe containing the ranges and bearings from the sensor system to every target at each time the position of the sensor system was measured. The output columns are a limited version of target_track_distance() and includes:
#' \itemize{
#'   \item{time: time of the measurement of ownship position}
#'   \item{ownShipTruthID: unique identifier for the reference platform}
#'   \item{ownShipLon: longitude of ownship at this time}
#'   \item{ownShipLat: latitude of ownship at this time}
#'   \item{ownShipAlt: altitude of ownship at this time}
#'   \item{slantRange: range from ownship to the target (straight-line distance)}
#'   \item{groundRange: range from ownship to the target (everyone's altitude is zeroed out)}
#'   \item{targetAspect: the target aspect (as seen from ownship) at this time}
#'   \item{trueBearingToTarget: true bearing (azimuth) to the target at this time}
#'   \item{relBearingToTarget: relative bearing (azimuth) to the target at this time}
#'   \item{targetTruthID: unique identifier for the target}
#'   \item{targetLon: target longitude at this time}
#'   \item{targetLat: target latitude at this time}
#'   \item{targetAlt: target altitude at this time}
#'   \item{targetHeading: target heading at this time}
#'   }
#'
#' @export
#'


# function that takes truth and radar data and returns the distances from all targets to all tracks
# using Benjamin's accuracy algorithm

# A copy of the above code, abbreviated to get target to ownship distance

target_ownship_distance <- function(truthData, ownShipData){
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

    verify_input_data(myData=truthData,columnNames=c("time","lat","lon","truthID","heading","alt"),factorNames="truthID", stopOnFail=TRUE)
    verify_input_data(myData=ownShipData,columnNames=c("time","lat","lon","truthID","heading","alt"),factorNames="truthID", stopOnFail=TRUE)

    ownShipTruthID=as.factor(unique(ownShipData$truthID))

    # we're going to build this list as we go through the loop
    # each entry will be one truth target compared to ownShip
    tempList=list()
    i=1

    #this bit just gets the error for every sensor point agianst every truth position - this is the 'dumb' part of the code, the clever part comes later
    for (target in (unique(truthData$truthID))) { #go through each target


        thisTargetTruth=filter(truthData,truthID==target) %>% #pull out data for this particular target
            filter(time <= max(ownShipData$time), time >= min(ownShipData$time)) %>% #pull out only times where ownShip existed
            mutate(truthID = as.character(truthID))

        localOwnShipData=ownShipData %>%
            filter(time <= max(thisTargetTruth$time), time >= min(thisTargetTruth$time)) #pull out only times where the targets existed

        if (nrow(localOwnShipData) == 0 ){
            s=sprintf("Your ownShip and target: %s do not overlap in time!\n",target)
            warning(s)
            next
        }

        if (any(duplicated(localOwnShipData$time))){
            s=sprintf("Your ownShip ('%s') has multiple lines with identical time values! (Maybe there was a rounding problem when you imported the data?) You must have unique times for each row.\n",ownShipTruthID)
            stop(s)
        }

        if (any(duplicated(thisTargetTruth$time))){
            s=sprintf("Your target ('%s') has multiple lines with identical time values! (Maybe there was a rounding problem when you imported the data?) You must have unique times for each row.\n",target)
            stop(s)
        }



        ########################################################################################
        ### Interpolate the truth data of the target to match the time of the ownship truth ###
        ########################################################################################

        ### we don't need to interpolate the target to times it ALREADY has, so let's figure out what's new ###
        uniqueOwnShipTimes=setdiff(localOwnShipData$time, thisTargetTruth$time)

        if (purrr::is_empty(uniqueOwnShipTimes)) { #this means that this target ALREADY has all target ownShip in it (no interpolation needed!)
            interpolatedTargetTruth=thisTargetTruth
        } else {
            # interpolate lat, lon, alt using WGS84.
            interpolatedEllipse <- interp_ellipse(thisTargetTruth, unlist(uniqueOwnShipTimes))

            #interpolate the truth target's heading to the ownship times.
            interpolatedTargetHeading=interp_heading(thisTargetTruth$time,thisTargetTruth$heading,uniqueOwnShipTimes)


            interpolatedTargetTruth=interpolatedEllipse %>%
                mutate(heading=interpolatedTargetHeading,
                       truthID=target) %>%
                bind_rows(thisTargetTruth) %>% #add back in the times we already had
                arrange(time) #sort by time for good measure
        }




        ###################################################################################
        ### Interpolate the truth data of ownShip to match the time of the target truth ###
        ###################################################################################

        ### we don't need to interpolate the target to times it ALREADY has, so let's figure out what's new ###
        uniqueTargetTimes=setdiff(thisTargetTruth$time,localOwnShipData$time)


        if (purrr::is_empty(uniqueTargetTimes)) { #this means that ownShip ALREADY has all target times in it (no interpolation needed!)
            interpolatedOwnShipTruth=localOwnShipData
        } else {
            # interpolate lat, lon, alt using WGS84.
            interpolatedEllipse <- interp_ellipse(localOwnShipData, unlist(uniqueTargetTimes))

            #interpolate the truth target's heading to the ownship times.
            interpolatedOwnShipHeading=interp_heading(localOwnShipData$time,localOwnShipData$heading,uniqueTargetTimes)


            interpolatedOwnShipTruth=interpolatedEllipse %>%
                mutate(heading=interpolatedOwnShipHeading,
                       truthID=unique(localOwnShipData$truthID)) %>%
                bind_rows(localOwnShipData) %>% #add back in the times we already had
                arrange(time) #sort by time for good measure

        }




        #gives get_relative_distance a matched list for ownShip and target with the same times
        tempList[[i]]=get_relative_distance(refPlatTruth=interpolatedOwnShipTruth, targetTruth=interpolatedTargetTruth)


        i=i+1

        ############################################################################################################
        ### Now that we know where everything was on the same timestamps, let's start calculating the difference ###
        ### between what the sensor reported and reality                                                         ###
        ############################################################################################################
        #
        #         # figure out the distance (in meters) between where the sensor THOUGHT the target was and where it REALLY was
        #         targetXYZ <- lla_to_xyz(interpolatedTargetLon, interpolatedTargetLat, interpolatedTargetAlt)
        #         ownShipXYZ <- lla_to_xyz(localOwnShipData$lon, localOwnShipData$lat, localOwnShipData$alt)
        #
        #         # range to ship is based on truth data and own ship data
        #         slantRange <- sqrt( (targetXYZ[[1]] - ownShipXYZ[[1]])^2 +
        #                                  (targetXYZ[[2]] - ownShipXYZ[[2]])^2 +
        #                                  (targetXYZ[[3]] - ownShipXYZ[[3]])^2 )
        #
        #         targetLL=cbind(interpolatedTargetLon, interpolatedTargetLat)
        #         ownShipLL=cbind(localOwnShipData$lon, localOwnShipData$lat)
        #         groundRange <- geosphere::distGeo(targetLL,ownShipLL)
        #
        #         # calculating (true) relative bearing of the target from the sensor's perspective (i.e., bearing - ship heading)
        #         trueBearingToTarget <- geosphere::bearing(cbind(localOwnShipData$lon, localOwnShipData$lat),
        #                                                   cbind(interpolatedTargetLon, interpolatedTargetLat)) %>%
        #             unlist()
        #         trueBearingToTarget <- trueBearingToTarget %% 360 # convert from -180/180 range to 0/360 range
        #         relBearingToTarget <- (trueBearingToTarget - localOwnShipData$heading) %% 360
        #
        #
        #         # get target aspect (this is the angle you are seeing the target - i.e. are you seeing its bow, beam, or stern)
        #         # heading - reverse bearing (target to ownship)
        #         targetAspect <- lapply(1:length(interpolatedTargetLon),
        #                                function(x) geosphere::bearing(c(interpolatedTargetLon[x], interpolatedTargetLat[x]),
        #                                                               c(localOwnShipData$lon[x], localOwnShipData$lat[x]))) %>%
        #             unlist()
        #         targetAspect <- ( targetAspect - interpolatedTargetHeading ) %% 360
        #
        #         #add the data as a new item in a list (see header comments for explanation of pointIndex!)
        #         tempList[[i]]=data.frame(time = localOwnShipData$time,ownShipTruthID=ownShipTruthID, targetTruthID = as.factor(target),
        #                                  ownShipLon = localOwnShipData$lon,ownShipLat = localOwnShipData$lat, ownShipAlt = localOwnShipData$alt,
        #                                  slantRange=slantRange, groundRange=groundRange,
        #                                  targetAspect, trueBearingToTarget,
        #                                  relBearingToTarget, targetLon = interpolatedTargetLon,
        #                                  targetLat = interpolatedTargetLat, targetAlt = interpolatedTargetAlt,
        #                                  targetHeading = interpolatedTargetHeading)


    }

    #combine the different entries
    targetOwnshipDistance=stats::na.omit(do.call(rbind,tempList))

    return(targetOwnshipDistance)
}

