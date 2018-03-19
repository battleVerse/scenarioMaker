#' @title Ownship data for example 1
#'
#' @description Example 1 has 8 targets and 14 tracks.  Targets move at realistic speeds.  Some are destroyed partway through the test
#'
#' @format A data frame with 526 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{heading}{Ownship heading (degrees azimuth)}
#'   \item{truthID}{Ownship ID is just "USS Good Guy" in every row (factor)}
#'
#' }
#'

"example1_ownShipData"



#' @title Sensor data for example 1
#'
#' @description Example 1 has 8 targets and 14 tracks.  Targets move at realistic speeds.  Some are destroyed partway through the test
#'
#' @format A data frame with 2312 observations and 5 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{trackNum}{Track ID (factor)}
#'
#' }
#'

"example1_sensorData"







#' @title Truth data for example 1
#'
#' @description Example 1 has 8 targets and 14 tracks.  Targets move at realistic speeds.  Some are destroyed partway through the test
#'
#' @format A data frame with 5010 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{heading}{Target heading (degrees azimuth)}
#'   \item{truthID}{Target ID for each measurement.  Factor.  Values are "A" through "H"}
#'
#' }
#'

"example1_truthData"


#' @title Truth data for example 1 (with data dropouts)
#'
#' @description Example 1 has 8 targets and 14 tracks.  Targets move at realistic speeds.  Some are destroyed partway through the test.  This is the same data as example1_truthData however some data dropouts have been included
#'
#' @format A data frame with 4076 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{heading}{Target heading (degrees azimuth)}
#'   \item{truthID}{Target ID for each measurement.  Factor.  Values are "A" through "H"}
#'
#' }
#'

"example1_truthData_dropout"


#' @title Engagement data for example 1
#'
#' @description Engagement data for example 1.  Used by SIMDIS and sandTable
#'
#' @format A data frame with 13 observations and 6 variables:
#' \describe{
#'   \item{time}{Time of weapon fire}
#'   \item{source}{Source originiating the shot}
#'   \item{target}{Target the shot was aimed at}
#'   \item{weapon}{Code for which weapon was used}
#'   \item{color}{Color in which to plot the shot}
#'   \item{kill}{Flag for whether target was killed by shot (0 or 1)}
#'
#' }
#'

"example1_engagementData"




#' @title Platform info for example 1
#'
#' @description Platform info for example 1.  Used by SIMDIS and sandTable
#'
#' @format A data frame with 23 observations and 4 variables:
#' \describe{
#'   \item{truthID}{Target ID for each measurement.  Factor.  Values are "A" through "H"}
#'   \item{platformIcon}{Icon used for each platform (Used by SIMDIS)}
#'   \item{platformType}{Type of platform (used by SIMDIS)}
#'   \item{trackColor}{Color of track}
#'
#' }
#'

"example1_platformInfo"





#' @title Example 1 scenario
#'
#' @description Example 1 scenario.  Example 1 has 8 targets and 14 tracks.  Targets move at realistic speeds.  Some are destroyed partway through the test
#'
#' @format A named list of 9 elements containing:
#' \describe{
#'   \item{scenarioName}{Scenario name (string)}
#'   \item{sensorData}{see help page for example1_sensorData for more details}
#'   \item{targetTruth}{see help page for example1_truthData for more details}
#'   \item{ownShipTruth}{see help page for example1_ownShipData for more details}
#'   \item{engagementData}{see help page for example1_engagementData for more details}
#'   \item{platformInfo}{see help page for example1_platformInfo for more details}
#'   \item{targetTrackDistance}{Calculated from targetTruth, sensorData, and ownShipTruth using the function target_track_distance().  See help page for target_track_distance() for more details}
#'   \item{targetOwnShipDistance}{Calculated from targetTruth and ownShipTruth using the function target_ownship_distance().  See help page for target_ownship_distance() for more details}
#'   \item{trackOwnShipDistance}{Calculated from sensorData and ownShipTruth using the function track_ownship_distance().  See help page for track_ownship_distance() for more details}
#'
#' }
#'

"example1_scenario"





#' @title Ownship data for example 2
#'
#' @description Example 2 has 2 targets and 1 track.
#'
#' @format A data frame with 300 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{heading}{Ownship heading (degrees azimuth)}
#'   \item{truthID}{Ownship ID is just "USS Good Guy" in every row (factor)}
#'
#' }
#'

"example2_ownShipData"



#' @title Sensor data for example 2
#'
#' @description Example 2 has 2 targets and 1 track.
#'
#' @format A data frame with 21 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{trackNum}{Track ID (factor)}
#'   \item{heading}{Ownship heading (degrees azimuth)}
#'
#' }
#'

"example2_sensorData"







#' @title Truth data for example 2
#'
#' @description Example 2 has 2 targets and 1 track.
#'
#' @format A data frame with 337 observations and 6 variables:
#' \describe{
#'   \item{lat}{Latitude (degrees)}
#'   \item{lon}{Longitude (degrees)}
#'   \item{time}{Time of position measurement (double)}
#'   \item{alt}{Altitude (meters)}
#'   \item{heading}{Target heading (degrees azimuth)}
#'   \item{truthID}{Target ID for each measurement.  Factor.  Values are "A" through "H"}
#'
#' }
#'

"example2_truthData"




#' @title Example 2 scenario
#'
#' @description Example 2 scenario.  Example 2 has 2 targets and 1 track.  Targets move at realistic speeds.
#'
#' @format A named list of 9 elements containing:
#' \describe{
#'   \item{scenarioName}{Scenario name (string)}
#'   \item{sensorData}{see help page for example2_sensorData for more details}
#'   \item{targetTruth}{see help page for example2_truthData for more details}
#'   \item{ownShipTruth}{see help page for example2_ownShipData for more details}
#'   \item{engagementData}{NA - This was not an input to the original call to create_scenario()}
#'   \item{platformInfo}{NA - This was not an input to the original call to create_scenario()}
#'   \item{targetTrackDistance}{Calculated from targetTruth, sensorData, and ownShipTruth using the function target_track_distance().  See help page for target_track_distance() for more details}
#'   \item{targetOwnShipDistance}{Calculated from targetTruth and ownShipTruth using the function target_ownship_distance().  See help page for target_ownship_distance() for more details}
#'   \item{trackOwnShipDistance}{Calculated from sensorData and ownShipTruth using the function track_ownship_distance().  See help page for track_ownship_distance() for more details}
#'
#' }
#'

"example2_scenario"

