#' @title Plot both sensor and truth data with altitude
#'
#' @description In data sets with nonzero altitude it may be useful to plot target altitude along with latitude and longitude. The central panel of this figure is the same as plot_raw_data(), but includes side panels that show the altitude of tracks and targets.
#'
#' This figure can be created before any tracks are assigned to targets to ensure that the user has imported the data correctly and to get an overall picture of the test.  Because it is a layout of objects it is more difficult to change the text labels than plot_sensor_and_truth_data().
#'
#' @param scenario MUST contain targetTruth, ownShipTruth, and sensorData
#' @param offset amount to offset labels (in degrees latitude)
#' @param textSize size of label text
#'
#' @import ggplot2
#'
#' @return layout of ggplots
#'
#' @export
#'
#' @examples
#' plot_sensor_and_truth_data_with_altitude(example1_scenario, offset = 0.01, textSize = 3)


# a few functions to help with plotting

#user facing


plot_sensor_and_truth_data_with_altitude = function(scenario, offset = 0.01, textSize = 3){

    if (!is.data.frame(scenario$targetTruth) || !is.data.frame(scenario$ownShipTruth)|| !is.data.frame(scenario$sensorData)){
        stop("To use this plotting function, the scenario must include all of the following: targetTruth, ownShipTruth, and sensorData.")
    }

    ownShipData=scenario$ownShipTruth
    truthData=scenario$targetTruth
    sensorData=scenario$sensorData

    # 2D lon/lat plot
    centerplot <- plot_sensor_and_truth_data(scenario, offset, textSize) +
        theme(legend.position="none")

    # Alt vs lon (top panel)
    toppanel <- ggplot() +
        geom_path(data=ownShipData,aes(x=lon, y=alt)) +
        geom_path(data=truthData,aes(x=lon,y=alt,color=truthID))+
        geom_point(data=sensorData,aes(x=lon,y=alt,color=trackNum))+
        geom_path(data=sensorData,aes(x=lon,y=alt,color=trackNum)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        theme(legend.position="none")



    # alt vs lat (right panel)
    rightpanel <- ggplot() +
        geom_path(data=ownShipData,aes(x=lat, y=alt)) +
        geom_path(data=truthData,aes(x=lat,y=alt,color=truthID))+
        geom_point(data=sensorData,aes(x=lat,y=alt,color=trackNum))+
        geom_path(data=sensorData,aes(x=lat,y=alt,color=trackNum)) +
        coord_flip() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(legend.position="none")


    # blank placeholder
    blankPlot <- ggplot()+geom_blank(aes(1,1))+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )


    myPlot=gridExtra::grid.arrange(toppanel, blankPlot, centerplot, rightpanel,
                                   ncol=2, nrow=2, widths=c(4, 2), heights=c(2, 4))

    return(myPlot)
}

