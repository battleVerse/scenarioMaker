#' @title Plot both sensor and truth data
#'
#' @description This figure shows an overview of the test and plots the ownship data (thin black line) along with the truth data (solid lines) and the tracks from the sensor (points with connecting lines). The target labels (likely characters or names) are placed above the the first point in time by an amount equal to the offset (in degrees latitude). The track labels (likely numbers) are placed below the last point in time of each track by the same amount. The size of the label text is given by textSize.
#'
#' This figure can be created before any tracks are assigned to targets to ensure that the user has imported the data correctly and to get an overall picture of the test.
#'
#' @param scenario MUST contain one of: targetTruth, ownShipTruth, and sensorData
#' @param offset (default = 0.01) amount to offset labels (in degrees latitude)
#' @param textSize (default = 3) size of label text
#' @param hideLegend hides the legend to make the graph easier to read (default=FALSE)
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_sensor_and_truth_data(example1_scenario, offset = 0.01, textSize = 3)


# a few functions to help with plotting

#user facing


plot_sensor_and_truth_data = function(scenario, offset = 0.01, textSize = 3,  hideLegend = FALSE, useDefaultColors=FALSE){

    if (!is.data.frame(scenario$targetTruth) & !is.data.frame(scenario$ownShipTruth) & !is.data.frame(scenario$sensorData) ){
        stop("To use this plotting function, the scenario must include at least one of the following: targetTruth, ownShipTruth, and sensorData.")
    }

    # ownShipData=scenario$ownShipTruth
    # truthData=scenario$targetTruth
    # sensorData=scenario$sensorData

    myPlot=ggplot()


    listOfIDs=c()

    ### Truth Data ###
    if (is.data.frame(scenario$targetTruth)){
        truthLabels <- scenario$targetTruth %>%
            group_by(truthID) %>%
            arrange(time) %>%
            summarise(lat = first(lat), lon = first(lon))

        myPlot = myPlot +
            geom_path(data=scenario$targetTruth,aes(x=lon,y=lat,color=truthID))+
            geom_label(data=truthLabels,aes(x=lon,y=lat + offset,label=truthID), size = textSize,alpha=.7)

        listOfIDs=c(listOfIDs,levels(scenario$targetTruth$truthID))

    }

    ### Sensor Data ###
    if (is.data.frame(scenario$sensorData)){

        sensorLabels <- scenario$sensorData %>%
            group_by(trackNum) %>%
            arrange(time) %>%
            summarise(lat = last(lat), lon = last(lon))

        myPlot = myPlot +
            geom_point(data=scenario$sensorData,aes(x=lon,y=lat,color=trackNum))+
            geom_path(data=scenario$sensorData,aes(x=lon,y=lat,color=trackNum)) +
            geom_label(data=sensorLabels,aes(x=lon,y=lat - offset,label=trackNum), size = textSize,alpha=.7)

        listOfIDs=c(listOfIDs,levels(scenario$sensorData$trackNum))

    }


    ### OwnShip Data ###
    if (is.data.frame(scenario$ownShipTruth)){
        myPlot = myPlot + geom_path(data=scenario$ownShipTruth,aes(x=lon, y=lat,color=truthID))
        listOfIDs=c(listOfIDs,levels(scenario$ownShipTruth$truthID))

    }

    myPlot = myPlot + labs(x = 'Longitude', y = 'Latitude', colour = "Track Number \n or Target ID")+
        coord_quickmap()

    defaultPlatColors=viridis::viridis(length(listOfIDs))
    colorNames=scenarioMaker:::get_named_colors(scenario,listOfIDs,defaultPlatColors,useDefaultColors=useDefaultColors)
    myPlot = myPlot + scale_color_manual(values=colorNames)


    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }

    return(myPlot)
}

