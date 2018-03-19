#' @title Geodesic lat lon offset
#'
#' @description This function is used by transform_offset_to_latlon() and it is not recommended that the user try to use this function as a stand-alone function.
#'
#' @param referenceLat reference latitude
#' @param referenceLon reference longitude
#' @param offsetLat offset latitude
#' @param offsetLon offset longitude
#'
#' @return A geodesic line (see geosphere package) between the reference point and the offset point calculated on a spheroidal earth (WGS84) by geosphere package.  This function is used by transform_offset_to_latlon() and is not recommended for general use
#'
#' @keywords internal
#'


geodesic_lat_long_offset = function(referenceLat, referenceLon, offsetLat, offsetLon){
        #not user facing
        angleToSensor=(atan2(offsetLon,offsetLat)*180/pi) %% 360 #because we've reversed the x & y arguments, we get 0 at north!
        rangeToSensor=sqrt(offsetLon^2+offsetLat^2)
        positions=cbind(referenceLon,referenceLat)

        return(geosphere::geodesic(positions,angleToSensor,rangeToSensor))
}

