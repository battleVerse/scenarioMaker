#' @title Get distance info between specified targets at the times provided
#'
#' @description Get distance info between specified targets at the times provided. Designed to work with exactly one truthID in each input dataframe. Both input dataframes MUST be on the same time base.  Be careful if you decide to use this; it is mostly intended as an internal function for distance_between() and target_ownship_distance().  If you're looking at this function, it's likely that you actually want one of those.
#'
#' @param refPlatTruth data frame containing all of the truth position of the reference platform (ownShip).  MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names.  This must contain ONE AND ONLY ONE VALUE FOR THIS FUNCTION}
#'  \item{heading: (double) sensor system heading in degrees azimuth}
#'}
#'
#' @param targetTruth truthData: data frame containing all of the truth data for each target. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names.  This must contain ONE AND ONLY ONE VALUE FOR THIS FUNCTION}
#'  \item{heading: (double) target heading in degrees azimuth}
#'  }
#'
#'
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
#'
#' @export
#'


get_relative_distance = function(refPlatTruth, targetTruth){

    if (!identical(refPlatTruth$time,targetTruth$time)){
        stop("The reference and target dataframes must have the exact same times in the exact same order.\n")
    }

    ownShipTruthID=as.factor(unique(refPlatTruth$truthID))
    #targetTruthID=as.factor(unique(targetTruth$truthID))

    # figure out the distance (in meters) between where the sensor THOUGHT the target was and where it REALLY was
    targetXYZ <- lla_to_xyz(targetTruth$lon, targetTruth$lat, targetTruth$alt)
    ownShipXYZ <- lla_to_xyz(refPlatTruth$lon, refPlatTruth$lat, refPlatTruth$alt)

    # range to ship is based on truth data and own ship data
    slantRange <- sqrt( (targetXYZ[[1]] - ownShipXYZ[[1]])^2 +
                            (targetXYZ[[2]] - ownShipXYZ[[2]])^2 +
                            (targetXYZ[[3]] - ownShipXYZ[[3]])^2 )

    targetLL=cbind(targetTruth$lon, targetTruth$lat)
    ownShipLL=cbind(refPlatTruth$lon, refPlatTruth$lat)
    groundRange <- geosphere::distGeo(targetLL,ownShipLL)

    # calculating (true) relative bearing of the target from the sensor's perspective (i.e., bearing - ship heading)
    trueBearingToTarget <- geosphere::bearing(cbind(refPlatTruth$lon, refPlatTruth$lat),
                                              cbind(targetTruth$lon, targetTruth$lat)) %>%
        unlist()
    trueBearingToTarget <- trueBearingToTarget %% 360 # convert from -180/180 range to 0/360 range
    relBearingToTarget <- (trueBearingToTarget - refPlatTruth$heading) %% 360


    # get target aspect (this is the angle you are seeing the target - i.e. are you seeing its bow, beam, or stern)
    # heading - reverse bearing (target to ownship)
    targetAspect <- lapply(1:length(targetTruth$lon),
                           function(x) geosphere::bearing(c(targetTruth$lon[x], targetTruth$lat[x]),
                                                          c(refPlatTruth$lon[x], refPlatTruth$lat[x]))) %>%
        unlist()
    targetAspect <- ( targetAspect - targetTruth$heading ) %% 360

    #add the data as a new item in a list (see header comments for explanation of pointIndex!)
    distData=data.frame(time = refPlatTruth$time,
                        ownShipTruthID=ownShipTruthID,
                        targetTruthID = targetTruth$truthID,
                        ownShipLon = refPlatTruth$lon,
                        ownShipLat = refPlatTruth$lat,
                        ownShipAlt = refPlatTruth$alt,
                        slantRange=slantRange,
                        groundRange=groundRange,
                        targetAspect,
                        trueBearingToTarget,
                        relBearingToTarget,
                        targetLon = targetTruth$lon,
                        targetLat = targetTruth$lat,
                        targetAlt = targetTruth$alt,
                        targetHeading = targetTruth$heading)

    return(distData)
}
