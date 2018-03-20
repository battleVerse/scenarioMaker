#' @title Plot both sensor and truth data with plotly
#'
#' @description This figure shows an overview of the test and plots the ownship data (thin black line) along with the truth data (solid lines) and the tracks from the sensor (points with connecting lines).  Targets and tracks are not labeled because they can be seen by hovering over a point in plotly figures.  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.
#'
#' This figure can be created before any tracks are assigned to targets to ensure that the user has imported the data correctly and to get an overall picture of the test.
#'
#' @param scenario MUST contain one of: targetTruth, ownShipTruth, and sensorData
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true
#' @param legendOnly (boolean, default = FALSE) Initially populates plot with only the legend.  Data can be turned on by selecting the data from the legend.  Good for plots with a large number of traces
#'
#'
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#' plot_sensor_and_truth_data_plotly(example1_scenario)


# a few functions to help with plotting

#user facing


plot_sensor_and_truth_data_plotly = function(scenario, useDefaultColors=FALSE, legendOnly = FALSE){

    if (!is.data.frame(scenario$targetTruth) & !is.data.frame(scenario$ownShipTruth) & !is.data.frame(scenario$sensorData) ){
        stop("To use this plotting function, the scenario must include at least one of the following: targetTruth, ownShipTruth, and sensorData.")
    }

    ### to get all the colors right, the simplest thing for us to do is combine ALL the dataframes into a single one ###
    ### we're doing something fancy and complicated here, so 'combine_target_ownship_truth' isn't appropriate ###
    ### the key is to pre-calculate the hovertext for each dataframe, and then combine them all into a single one for plotting ###

    #############################
    ### Get Target Data Ready ###
    #############################



    if (is.data.frame(scenario$targetOwnShipDistance)) { #if you have both target and ownShip truth (and the derived dataframe), we can plot something more interesting!

        targetTruth=scenario$targetOwnShipDistance %>%
            mutate(hoverText=paste('Target: ',targetTruthID,'<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                   '<br>Lat: ',round(targetLat,5),
                                   '<br>Lon: ',round(targetLon,5),
                                   '<br>Alt (m): ',round(targetAlt,2),
                                   '<br>Slant Range to OwnShip (m): ',round(slantRange,2))
            ) %>%
            rename(lon=targetLon,lat=targetLat,uniqueID=targetTruthID) %>% #rename for joining later
            mutate(drawColor="blue", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
            select(time, lon, lat,uniqueID, hoverText, drawColor)


    } else if (is.data.frame(scenario$targetTruth)){ #if you only have target truth, plot the simpler thing
        targetTruth=scenario$targetTruth %>%
            mutate(hoverText=paste('Target: ',truthID,'<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                   '<br>Lat: ',round(lat,5),
                                   '<br>Lon: ',round(lon,5),
                                   '<br>Alt (m): ',round(alt,2))
            )%>%
            rename(uniqueID=truthID) %>% #rename for joining later
            mutate(drawColor="blue", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
            select(time, lon, lat,uniqueID, hoverText, drawColor)
    } else { #if missing entirely, make empty data frame so we can 'bind_rows' later
        targetTruth=data.frame(time=NA,lon=NA,lat=NA,uniqueID=NA,hoverText=NA,drawColor=NA)
    }

    #############################
    ### Get Sensor Data Ready ###
    #############################

    if (is.data.frame(scenario$trackOwnShipDistance)) {#if you have both sensor and ownShip truth, we can plot something more interesting!
        sensorData=scenario$trackOwnShipDistance %>%
            mutate(hoverText=paste('Track #: ',trackNum,
                                   '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                   '<br>Lat: ',round(trackLat,5),
                                   '<br>Lon: ',round(trackLon,5),
                                   '<br>Alt (m): ',round(trackAlt,2),
                                   '<br>Slant Range to OwnShip (m): ',round(slantRange,2))
            ) %>%
            rename(uniqueID=trackNum, lon=trackLon, lat=trackLat) %>% #rename for joining later
            mutate(drawColor="red", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
            select(time, lon, lat,uniqueID, hoverText, drawColor)

    } else if (is.data.frame(scenario$sensorData)) { #if you only have sensor data, plot the simpler thing
        sensorData=scenario$sensorData %>%
            mutate(hoverText=paste('Track #: ',trackNum,
                                   '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                   '<br>Lat: ',round(lat,5),
                                   '<br>Lon: ',round(lon,5),
                                   '<br>Alt (m): ',round(alt,2))
            ) %>%
            rename(uniqueID=trackNum) %>% #rename for joining later
            mutate(drawColor="red", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
            select(time, lon, lat,uniqueID, hoverText, drawColor)
    } else { #if missing entirely, make empty data frame so we can 'bind_rows' later
        sensorData=data.frame(time=NA,lon=NA,lat=NA,uniqueID=NA,hoverText=NA,drawColor=NA)
    }


    ##############################
    ### Get OwnShip Data Ready ###
    ##############################

    if (is.data.frame(scenario$ownShipTruth)){
        ownShipTruth=scenario$ownShipTruth %>%
            mutate(hoverText= paste('Target: ',truthID,'<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                    '<br>Lat: ',round(lat,5),
                                    '<br>Lon: ',round(lon,5),
                                    '<br>Alt (m): ',round(alt,2))) %>%
            rename(uniqueID=truthID) %>% #rename for joining later
            mutate(drawColor="black", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
            select(time, lon, lat,uniqueID, hoverText, drawColor)
    } else { #if missing entirely, make empty data frame so we can 'bind_rows' later
        ownShipTruth=data.frame(time=NA,lon=NA,lat=NA,uniqueID=NA,hoverText=NA,drawColor=NA)
    }



    #################################
    ### Combine Data for Plotting ###
    #################################

    combinedData=bind_rows(list(targetTruth,sensorData,ownShipTruth)) %>% #combine all the data
        stats::na.omit() %>% #omit missing data (caused by entirely missing one of the dataframes)
        mutate(uniqueID = as.factor(uniqueID)) #recast as factor for plotting

    #get the named colors
    defaultPlatColors=combinedData %>% group_by(uniqueID) %>% slice(1) %>% ungroup() %>% select(drawColor) #get the first row for each uniqueID and get the color
    defaultPlatColors = c(defaultPlatColors$drawColor)
    colorNames=get_named_colors(scenario,levels(combinedData$uniqueID),defaultPlatColors,useDefaultColors=useDefaultColors)


    myPlot = plotly::plot_ly(data=combinedData,
                     x=~lon,
                     y=~lat,
                     type='scatter',
                     mode='lines',
                     color=~uniqueID,
                     colors=colorNames,
                     hoverinfo='text',
                     text=~hoverText,
                     visible = ifelse(legendOnly, 'legendonly', TRUE)
    )







    myPlot = myPlot %>% plotly::layout(xaxis=list(title="Longitude"),
                               yaxis=list(title="Latitude"))


    return(myPlot)
}

