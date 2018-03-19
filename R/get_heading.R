#' @title Get heading based on lon and lat
#'
#' @description Nautilus calculates errors as a function of bearing and target aspect and therefore requires heading (or bearing) for ownship and for each target. If the truth data already contains this information then this function is unnecessary. If not, then the user will be required to run get_heading() before inputting the data into target_track_distance().
#'
#' get_heading() uses the bearing() function from the package geosphere. For each point, get_heading() calculates the bearing between the current point and the previous point as well as the current point and the following point. get_heading then averages these two numbers and returns the average as the bearing column of the output.
#'
#' @param dataSet data frame with lon, lat, time, and target/track identifiers
#' @param truthIDColName (default="truthID") (string) the name of the column with target/track identifiers
#'
#' @return dataSet with an additional column, heading, that is the average heading at each point.
#'
#' @export
#'
#' @examples
#' get_heading(example1_scenario$targetTruth, "truthID")


get_heading <- function(dataSet, truthIDColName="truthID"){
        #user facing

        ################################################################################################
        ### Description: This is a "helper" function that will take time, lat, lon and
        ### generate heading in degrees (North = 0). Uses nearest neighbor points on either side
        ### (i.e. my heading at time t is the average of the bearing between t-1 to t and t to t+1).
        ### Intended use is adding heading to truth data if you don't already have it prior to using
        ### the main radar code.
        ###
        ### Important! If you have multiple objects (e.g. ships) in this dataset, you must use the
        ### groupColumn entry to select the target IDs
        ###
        ### Input: dataframe with 'time' (ascending number), 'lat' (degrees), 'lon' (degrees), and
        ### 'truthID' (factor) columns,
        ###
        ### Output: the same dataframe with a heading (degrees) column. North = 0, goes clockwise
        ###
        ################################################################################################


        # create new columns for lag and lead lat and lon
        dataSet <- dataSet %>%
                group_by_(truthIDColName) %>%
                arrange(time) %>% #ensure that everything is in order!
                mutate(laglat = lag(lat), laglon = lag(lon), leadlat = lead(lat), leadlon = lead(lon))

        # find bearing between previous and current point, then find bearing between current and next point
        dataSet$heading1 <- geosphere::bearing(cbind(dataSet$laglon, dataSet$laglat),
                                              cbind(dataSet$lon, dataSet$lat))

        dataSet$heading2 <- geosphere::bearing(cbind(dataSet$lon, dataSet$lat),
                                              cbind(dataSet$leadlon, dataSet$leadlat))

        # take the mean of each pair of values.
        dataSet$heading <- rowMeans(cbind(dataSet$heading1, dataSet$heading2), na.rm = TRUE)

        # NOTE: if bearing is ~ 180 deg, you can have heading 1 ~ +179.99 and heading 2 ~ -179.99,  This gives errors.  Here we fix those by taking the mean only when the bearings are the same sign.  Otherwise, we return 180 deg. + sum/2 of points.  It ignores when the sign change occurs around 0 (i.e., -1 to +1) by requiring the |angle| > 90.
        problemRows <- which((sign(dataSet$heading1) != sign(dataSet$heading2)) & abs(dataSet$heading1) > 90)
        dataSet$heading[problemRows] <- 180 + (dataSet$heading1[problemRows] + dataSet$heading2[problemRows])/2

        # convert from -180/180 range to 0/360 range
        dataSet$heading <- ifelse(dataSet$heading < 0 ,
                                  360 + dataSet$heading,
                                  dataSet$heading)

        # drop those extra columns
        dataSet <- dataSet %>%
                select(-leadlat, -leadlon, -laglat, -laglon, -heading1, -heading2) %>%
            ungroup()

        return(dataSet)
}



