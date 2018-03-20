#' @title Plot truth and/or sensor data relative to ownship
#'
#' @description This figure shows the bearing and range of targets and/or tracks relative to the sensor system (ownship).
#'
#'
#'
#' @param scenario must contain either or both: targetOwnshipDistance, trackOwnshipDistance
#' @param offset (default = 1) amount to offset labels (in degrees)
#' @param textSize (default = 3) size of label text
#' @param hideLegend hides the legend to make the graph easier to read (default=FALSE)
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#' @param plotWhich (default = 'both') also accepts 'sensor' or 'truth'.  Plots only truth-ownship or track-ownship or plots both.
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_relative_truth_and_sensor(example1_scenario, offset = 1, textSize = 3)


# a few functions to help with plotting

#user facing


plot_relative_truth_and_sensor = function(scenario, offset = 1, textSize = 3, hideLegend = FALSE, useDefaultColors=FALSE, plotWhich = 'both'){

    if (!is.data.frame(scenario$targetOwnShipDistance) && !is.data.frame(scenario$targetOwnShipDistance)){
        stop("To use this plotting function, the scenario must include target-ownship distance and/or track-ownship distance data.")
    }

    # start the plot object
    myPlot=ggplot()

    # start the list of IDs used for colors
    listOfIDs=c()

    maxRange=0

    if(is.data.frame(scenario$targetOwnShipDistance) & plotWhich %in% c('both', 'truth')){
        truthLabels <- scenario$targetOwnShipDistance %>%
            group_by(targetTruthID) %>%
            arrange(time) %>%
            summarise(slantRange = first(slantRange), relBearingToTarget = first(relBearingToTarget))

        myPlot = myPlot +
            geom_path(data = scenario$targetOwnShipDistance,
                      aes(x = relBearingToTarget, y=slantRange, color=targetTruthID))+
            geom_point(data = scenario$targetOwnShipDistance,
                      aes(x = relBearingToTarget, y=slantRange, color=targetTruthID))+
            geom_label(data = truthLabels,
                       aes(x = relBearingToTarget + offset, y = slantRange, label = targetTruthID),
                       size = textSize,
                       alpha=.7)

        maxRange=max(maxRange, scenario$targetOwnShipDistance$slantRange)

        listOfIDs <- c(listOfIDs, levels(scenario$targetOwnShipDistance$targetTruthID))
    }


    if(is.data.frame(scenario$trackOwnShipDistance) & plotWhich %in% c('both', 'sensor')){
        trackLabels <- scenario$trackOwnShipDistance %>%
            group_by(trackNum) %>%
            arrange(time) %>%
            summarise(slantRange = first(slantRange), relBearingToTrack = first(relBearingToTrack))

        myPlot = myPlot +
            geom_path(data = scenario$trackOwnShipDistance,
                      aes(x = relBearingToTrack, y=slantRange, color=trackNum))+
            geom_point(data = scenario$trackOwnShipDistance,
                       aes(x = relBearingToTrack, y=slantRange, color=trackNum))+
            geom_label(data = trackLabels,
                       aes(x = relBearingToTrack + offset, y = slantRange, label = trackNum),
                       size = textSize,
                      alpha=.7)

        maxRange=max(maxRange, scenario$trackOwnShipDistance$slantRange)

        listOfIDs <- c(listOfIDs, levels(scenario$trackOwnShipDistance$trackNum))

    }

    myPlot <- myPlot +
        coord_polar(theta = 'x', start = (0)) +
        scale_x_continuous(limits=c(0, 360), breaks=c(0, 45, 90, 135, 180, 225, 270, 315)) +
        scale_y_continuous(limits=c(0,maxRange))+
        labs(x = "", y = "Range to Ship")

    # get fancy new colors
    defaultPlatColors=viridis::viridis(length(listOfIDs))
    colorNames= get_named_colors(scenario,listOfIDs,defaultPlatColors,useDefaultColors=useDefaultColors)
    myPlot = myPlot + scale_color_manual(values=colorNames)


    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }


    return(myPlot)
}

