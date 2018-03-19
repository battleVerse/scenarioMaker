#' @title Plot distance data with plotly
#'
#' @description This figure plots target range as a function of time.  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.
#'
#' @param scenario must contain the output of target_ownship_distance() as a named object in the scenario list with the name "targetOwnShipDistance".  If you originally ran create_scenario() with both own ship data and target truth data then this should already be in the scenario.
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#' @param legendOnly (boolean, default = FALSE) Initially populates plot with only the legend.  Data can be turned on by selecting the data from the legend.  Good for plots with a large number of traces
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#'
#' plot_distance_data_plotly(example1_scenario, useDefaultColors = FALSE)


# a few functions to help with plotting

#user facing


plot_distance_data_plotly = function(scenario, useDefaultColors=FALSE, legendOnly = FALSE){

    if (!is.data.frame(scenario$targetOwnShipDistance)){ #if targetOwnShipDistance isn't present
        stop("This scenario is does not have targetOwnShipDistance pre-calculated. Are you sure you built the scenario with both ownship and target truth data?")
    }

    distanceData=scenario$targetOwnShipDistance

    ylabel=list(title="Target Range to Ownship (meters)")
    xlabel=list(title="Time")

    distanceData=mutate(distanceData,time=as.POSIXct(time,tz="UTC",origin="1970-01-01"))

    defaultPlatColors=viridis::viridis(length(levels(distanceData$targetTruthID)))
    colorNames=get_named_colors(scenario,levels(distanceData$targetTruthID),defaultPlatColors,useDefaultColors=useDefaultColors)


    myPlot=plotly::plot_ly(data=distanceData,
            type='scatter',
            mode='lines',
            x=~time,
            y=~slantRange,
            color=~targetTruthID,
            colors=colorNames,
            hoverinfo='text',
            text=~paste('Relative Distance',
                        '<br>Time: ', strftime(as.POSIXct(time,tz="UTC",origin="1970-01-01"),"%H:%M:%OS2"),
                        '<br>Target Name: ',targetTruthID,
                        '<br>Distance to Sensor (m): ',round(slantRange,2),
                        '<br>Target Aspect (deg):', round(targetAspect,2)),
            visible = ifelse(legendOnly, 'legendonly', TRUE)) %>%
        plotly::layout(xaxis=xlabel,yaxis=ylabel)



    return(myPlot)

}

