#' @title Plot truth and/or sensor data relative to ownship - plotly version
#'
#' @description This figure shows the bearing and range of targets and/or tracks relative to the sensor system (ownship).
#'
#'
#'
#' @param scenario must contain either or both: targetOwnshipDistance, trackOwnshipDistance
#' @param offset (default = 1) amount to offset labels (in degrees)
#' @param textSize (default = 3) size of label text
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#' @param plotWhich (default = 'both') also accepts 'sensor' or 'truth'.  Plots only truth-ownship or track-ownship or plots both.
#' @param legendOnly (boolean, default = FALSE) Initially populates plot with only the legend.  Data can be turned on by selecting the data from the legend.  Good for plots with a large number of traces
#'
#'
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#' plot_relative_truth_and_sensor_plotly(example1_scenario, offset = 1, textSize = 3)


#user facing

plot_relative_truth_and_sensor_plotly = function(scenario, offset = 1, textSize = 3, useDefaultColors=FALSE, plotWhich = 'both', legendOnly = FALSE){

    if (!is.data.frame(scenario$targetOwnShipDistance) && !is.data.frame(scenario$targetOwnShipDistance)){
        stop("To use this plotting function, the scenario must include target-ownship distance and/or track-ownship distance data.")
    }

    # start the list of data frames that will be used in plot object
    listOfDataFrames=list()

    # start the list of IDs used for colors
    listOfIDs=c()


    if(is.data.frame(scenario$targetOwnShipDistance) & plotWhich %in% c('both', 'truth')){
        truthLabels <- scenario$targetOwnShipDistance %>%
            group_by(targetTruthID) %>%
            arrange(time) %>%
            summarise(slantRange = first(slantRange), relBearingToTarget = first(relBearingToTarget))

        thisData <- scenario$targetOwnShipDistance %>%
            select(time, relBearingToTarget, slantRange, targetTruthID) %>%
            rename(bearing = relBearingToTarget, targetOrTrackID = targetTruthID)

        listOfDataFrames[[1]] <- thisData

        listOfIDs <- c(listOfIDs, levels(scenario$targetOwnShipDistance$targetTruthID))
    }


    if(is.data.frame(scenario$trackOwnShipDistance) & plotWhich %in% c('both', 'sensor')){
        trackLabels <- scenario$trackOwnShipDistance %>%
            group_by(trackNum) %>%
            arrange(time) %>%
            summarise(slantRange = first(slantRange), relBearingToTrack = first(relBearingToTrack))

        thisData <- scenario$trackOwnShipDistance %>%
            select(time, relBearingToTrack, slantRange, trackNum) %>%
            rename(bearing = relBearingToTrack, targetOrTrackID = trackNum)

        listOfDataFrames[[2]] <- thisData

        listOfIDs <- c(listOfIDs, levels(scenario$trackOwnShipDistance$trackNum))

    }

    # merge the two data sets into one
    thisPlotData <- do.call(rbind, listOfDataFrames)

    # get fancy new colors
    defaultPlatColors = viridis::viridis(length(listOfIDs))
    colorNames = scenarioMaker:::get_named_colors(scenario,
                                                  listOfIDs,
                                                  defaultPlatColors,
                                                  useDefaultColors = useDefaultColors)




    myPlot = plotly::plot_ly(data = thisPlotData,
                     type = 'scatter',
                     mode = 'markers',
                     r = ~slantRange,
                     t = ~bearing,
                     color = ~targetOrTrackID,
                     colors = colorNames,
                     alpha = 0.5,
                     hoverinfo = 'text',
                     text = ~paste('Relative Distance',
                                 '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                 '<br>Target Name: ',targetOrTrackID,
                                 '<br>Distance to Sensor (m): ',round(slantRange,2),
                                 '<br>Bearing (deg):', round(bearing,2)),
                     visible = ifelse(legendOnly, 'legendonly', TRUE)) %>%
        plotly::layout(orientation = -90)


    return(myPlot)
}

