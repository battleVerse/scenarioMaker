#' @title Convert cartesian coordinates to Earth lon/lat/alt
#'
#' @description This function converts cartesian coordinates to Earth lat/lon/alt (WGS84).  Cartesian coordinates are from the center of Earth.  Meant to be used in conjunction with \code{\link{lla_to_xyz}}
#'
#' @param x cartesian x
#' @param y cartesian y
#' @param z cartesian z
#'
#' @return A list of 3 numbers where the 3 entries correspond to lon (degrees), lat (degrees), and altitude (meters) on Earth
#'
#' @examples
#' scenarioMaker:::xyz_to_lla(12000, 24000, 36000)
#'
#' @keywords internal


#not user facing

### Functions to convert ellipse data to cartesian and back

xyz_to_lla <- function(x, y, z){
        # This function takes x, y, z, converts to lon, lat, alt using WGS84.
        # INPUTS:
        #  3 numbers (x, y, and z)

        # Output:
        #  a vector of 3 numbers: lon, lat, and alt

        # convert xyz back to lat, lon, alt
        # taken from MATLAB Geodetic Toolbox (geodetic299, xyz2ell2)
        # Uses direct algorithm in B.R. Bowring, "The accuracy of geodetic latitude and height equations", Survey Review, v28 #218, October 1985, pp.202-206.

        # a and f for WGS84
        a <- 6378137  # semi-major axis
        b <- 6356752.314245 # semi-major axis
        f <-  1/298.257223563  # flattening
        e2 <- 2*f - f^2  # Square of the eccentricity

        lon = atan2(y ,x)*180/pi
        e = e2*(a/b)^2
        p = sqrt(x^2 + y^2)
        r = sqrt(p^2 + z^2)
        u = atan(b * z * (1+e * b / r)/(a * p))
        lat = atan((z + e * b * sin(u)^3) / (p - e2*a*cos(u)^3))*180/pi
        v = a / sqrt(1-e2*sin(lat*pi/180)^2)
        alt = p*cos(lat*pi/180) + z * sin(lat*pi/180)- a^2/v

        return(list(lon, lat, alt))

}


#lla_to_xyz(-121, 45, 100)
#lla_to_xyz(c(-121, -122), c(45, 44), c(100, 102))

#ptxyz <- lla_to_xyz(-121, 45, 100)
#xyz_to_lla(ptxyz[1], ptxyz[2], ptxyz[3])

#ptxyz <- lla_to_xyz(c(-121, -122), c(45, 44), c(100, 102))
#xyz_to_lla(ptxyz[[1]], ptxyz[[2]], ptxyz[[3]])

