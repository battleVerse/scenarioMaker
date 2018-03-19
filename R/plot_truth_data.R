#' @title Plot truth data
#'
#' @description This figure shows an overview of the test and plots the ownship data (thin black line) and/or the truth data (solid lines). The target labels (likely characters or names) are placed above the the first point in time by an amount equal to the offset (in degrees latitude). The size of the label text is given by textSize.
#'
#'
#'
#' @param scenario must contain either or both: targetTruth, ownShipTruth
#' @param offset amount to offset labels (in degrees latitude)
#' @param textSize size of label text
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
#' plot_truth_data(example1_scenario, offset = 0.01, textSize = 3, hideLegend = FALSE, useDefaultColors = FALSE)


# a few functions to help with plotting

#user facing


plot_truth_data = function(scenario, offset = 0.01, textSize = 3, hideLegend = FALSE, useDefaultColors=FALSE){

    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this plotting function, the scenario must include target and/or ownship truth data.")
    }


    myPlot=ggplot()

    listOfIDs=c()
    ### Truth data ###
    if (is.data.frame(scenario$targetTruth)){
        truthLabels <- scenario$targetTruth %>%
            group_by(truthID) %>%
            arrange(time) %>%
            summarise(lat = first(lat), lon = first(lon))



        myPlot = myPlot +
            geom_path(data=scenario$targetTruth,aes(x=lon,y=lat,color=truthID))+
            geom_point(data=scenario$targetTruth,aes(x=lon,y=lat,color=truthID))+
            geom_label(data=truthLabels,aes(x=lon,y=lat + offset,label=truthID), size = textSize,alpha=.7)+
            coord_quickmap()+
            xlab("Longitude")+ylab("Latitude")

        listOfIDs=c(listOfIDs,levels(scenario$targetTruth$truthID))
    }

    if (is.data.frame(scenario$ownShipTruth)){


        myPlot = myPlot +
            geom_path(data=scenario$ownShipTruth,aes(x=lon, y=lat,color=truthID))

        listOfIDs=c(listOfIDs,levels(scenario$ownShipTruth$truthID))

    }

    defaultPlatColors=viridis::viridis(length(listOfIDs))
    colorNames = scenarioMaker:::get_named_colors(scenario, listOfIDs, defaultPlatColors,
                                                  useDefaultColors = useDefaultColors)
    myPlot = myPlot + scale_color_manual(values=colorNames)

    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }


    return(myPlot)
}

