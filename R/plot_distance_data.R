#' @title Plot distance data
#'
#' @description This figure plots target range as a function of time
#'
#' @param scenario must contain the output of target_ownship_distance() as a named object in the scenario list with the name "targetOwnShipDistance".  If you originally ran create_scenario() with both own ship data and target truth data then this should already be in the scenario.
#' @param hideLegend hides the legend.  May make the graph easier to read in scenarios with a lot of targets (default=FALSE)
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true.  If you do not have platformInfo, it will use default colors no matter what.
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_distance_data(example1_scenario, hideLegend = FALSE, useDefaultColors = FALSE)
#'


# a few functions to help with plotting

#user facing


plot_distance_data = function(scenario, hideLegend = FALSE, useDefaultColors=FALSE){

    if (!is.data.frame(scenario$targetOwnShipDistance)){ #if targetOwnShipDistance isn't present
        stop("This scenario is does not have targetOwnShipDistance pre-calculated. Are you sure you built the scenario with both ownship and target truth data?")
    }

    distanceData=scenario$targetOwnShipDistance
    distanceData=mutate(distanceData,time=as.POSIXct(time,tz="UTC",origin="1970-01-01"))
    myPlot=ggplot(distanceData)+geom_line(aes(x=time,y=slantRange,color=targetTruthID))+
        xlab("Time")+ylab("Range to Ownship (meters)")

    defaultPlatColors=viridis::viridis(length(levels(distanceData$targetTruthID)))
    colorNames=get_named_colors(scenario,levels(distanceData$targetTruthID),defaultPlatColors,useDefaultColors=useDefaultColors)

    myPlot = myPlot + scale_color_manual(values=colorNames)

    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }


    return(myPlot)

}

