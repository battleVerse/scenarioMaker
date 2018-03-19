#' @title Plot truth data with plotly
#'
#' @description This function shows an overview of the test and plots the ownship data and/or the truth data in a plotly figure.  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.
#'
#'
#'
#' @param scenario must contain either or both: targetTruth, ownShipTruth
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#' @param legendOnly (boolean, default = FALSE) Initially populates plot with only the legend.  Data can be turned on by selecting the data from the legend.  Good for plots with a large number of traces
#'
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#' plot_truth_data_plotly(scenarioMaker::example1_scenario)


# a few functions to help with plotting

#user facing


plot_truth_data_plotly = function(scenario, useDefaultColors=FALSE, legendOnly = FALSE){

    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this plotting function, the scenario must include target and/or ownship truth data.")
    }

    xlabel=list(title="Longitude")
    ylabel=list(title="Latitude")

    myPlot=plotly::plot_ly()

    ### plot target truth data ###
    if (is.data.frame(scenario$targetTruth)){

        ### let's get the colors! ###
        defaultPlatColors=viridis::viridis(length(levels(scenario$targetTruth$truthID)))
        colorNames=get_named_colors(scenario,levels(scenario$targetTruth$truthID),defaultPlatColors,useDefaultColors=useDefaultColors)

        myPlot=myPlot %>%
            plotly::add_trace(data=scenario$targetTruth,
                      x=~lon,
                      y=~lat,
                      type='scatter',
                      mode='lines+markers',
                      color=~truthID,
                      colors=colorNames,
                      marker=list(size=4),
                      #legendgroup="Targets",
                      #line = list(color = 'rgba(0,0,255,1)'),
                      hoverinfo='text',
                      text=~paste('Target: ',truthID,'<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                  '<br>Lat: ',round(lat,5),
                                  '<br>Lon: ',round(lon,5),
                                  '<br>Alt (m): ',round(alt,2)),
                      visible = ifelse(legendOnly, 'legendonly', TRUE))
    }



    ### plot ownShip truth data ###
    if (is.data.frame(scenario$ownShipTruth)){

        defaultPlatColors="black"
        colorNames=get_named_colors(scenario,levels(scenario$ownShipTruth$truthID),defaultPlatColors,useDefaultColors=useDefaultColors)

        myPlot=myPlot %>%
            plotly::add_trace(data=scenario$ownShipTruth,
                      x=~lon,
                      y=~lat,
                      type='scatter',
                      mode='lines+markers',
                      split=~truthID,
                      marker=list(size=4),
                      line = list(color = colorNames),
                      hoverinfo='text',
                      text=~paste('Ownship Location','<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                  '<br>Lat: ',round(lat,5),
                                  '<br>Lon: ',round(lon,5),
                                  '<br>Alt (m): ',round(alt,2)),
                      visible = ifelse(legendOnly, 'legendonly', TRUE))
    }

    myPlot=  myPlot %>%
        plotly::layout(xaxis=xlabel,yaxis=ylabel)

    return(myPlot)
}

