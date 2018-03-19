#' @title Convert Earth lon/lat/alt to cartesian coordinates
#'
#' @description This function converts Earth latitude/longitude/altitude to cartesian coordinates (for interpolation functions). Cartesian coordinates are from the center of Earth.  Meant to be used in conjunction with \code{\link{xyz_to_lla}}
#'
#' @param lon Longitude in degrees
#' @param lat Latitude in degrees
#' @param alt Altitude in meters
#'
#' @return A list of 3 numbers where the 3 entries correspond to x, y, and z cartesian coordinates
#'
#' @examples
#' scenarioMaker:::lla_to_xyz(120, 42, 1005)
#'
#' @keywords internal

lla_to_xyz <- function(lon, lat, alt){
        # This function takes lat, lon, alt, converts to x,y,z using WGS84.
        # INPUTS:
        #  3 numbers or vectors: lon, lat, and alt

        # Output: a list of 3 numbers where each element is a vector of x, y, and z

        # a and f for WGS84
        a <- 6378137  # semi-major axis
        b <- 6356752.314245 # semi-major axis
        f <-  1/298.257223563  # flattening
        e2 <- 2*f - f^2  # Square of the eccentricity

        # calculate x,y,z for each point
        N = a / sqrt(1- e2*(sin(lat*pi/180))^2)
        x = (N + alt) * cos(lat*pi/180) * cos(lon*pi/180)
        y = (N + alt) * cos(lat*pi/180) * sin(lon*pi/180)
        z = (N * (1-e2) + alt) * sin(lat*pi/180)

        return(list(x, y, z))
}


#lla_to_xyz(-121, 45, 100)
#lla_to_xyz(c(-121, -122), c(45, 44), c(100, 102))

#ptxyz <- lla_to_xyz(-121, 45, 100)
#xyz_to_lla(ptxyz[1], ptxyz[2], ptxyz[3])

#ptxyz <- lla_to_xyz(c(-121, -122), c(45, 44), c(100, 102))
#xyz_to_lla(ptxyz[[1]], ptxyz[[2]], ptxyz[[3]])
