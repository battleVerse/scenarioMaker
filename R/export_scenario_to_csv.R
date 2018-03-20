#' @title Export contents of scenario to csv
#'
#' @description Export contents of scenario to csv.  Each data frame in the list is saved as a separate .csv
#'
#'
#' @param scenario will save the following dataframes to disk (ignoring those not present in the scenario): targetTruth, ownShipTruth, sensorData, engagementData, platformInfo, targetOwnShipDistance
#' @param saveLocation string - path to save file
#' @param saveName string (default=NULL) if no argument passed, will use the scenario's scenarioName property
#'

#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' To use this function, simply enter a valid path for 'saveLocation'
#' export_scenario_to_csv(example1_scenario,
#'  saveLocation="C:/Documents/analysis/",
#'  saveName="testPeriod1")
#' }


export_scenario_to_csv = function(scenario,saveLocation,saveName=NULL){



    ### Check to see if user remembered trailing slash on file location. If not, add it for them ###
    lastChar=stringr::str_sub(saveLocation,-1,-1)
    if (lastChar != "/"){
        saveLocation=sprintf("%s/",saveLocation)
    }

    if (is.null(saveName)){
        saveName=scenario$scenarioName
    }

    ### Make sure the scenario name doesn't have any forbidden characters ###
    forbiddenCharacters=c("<",">",":","/","\\","|","?","*")
    forbiddenMatches=intersect(strsplit(saveName,"")[[1]],forbiddenCharacters)
    if (length(forbiddenMatches)>0){
        listOfBad=paste(unlist(forbiddenMatches),collapse=", ")
        s=sprintf("Your scenario's name: '%s' contains the symbol(s): %s\nThese cannot be used as a file name in Windows. Either change the scenario's name (the 'scenarioName' property), or use the argument 'saveName' when calling this function.",scenario$scenarioName,listOfBad)
        stop(s)
    }

    ### Combine the output location with the scenario name ###
    saveLocation=sprintf("%s%s",saveLocation,saveName)


    ### Save targetTruth ###
    if (!any(is.na(scenario$targetTruth))){
        outName=sprintf("%s-targetTruth.csv",saveLocation)
        utils::write.csv(scenario$targetTruth,outName, row.names=FALSE)
    }

    ### Save ownShipTruth ###
    if (!any(is.na(scenario$ownShipTruth))){
        outName=sprintf("%s-ownShipTruth.csv",saveLocation)
        utils::write.csv(scenario$ownShipTruth,outName, row.names=FALSE)
    }

    ### Save sensorData ###
    if (!any(is.na(scenario$sensorData))){
        outName=sprintf("%s-sensorData.csv",saveLocation)
        utils::write.csv(scenario$sensorData,outName, row.names=FALSE)
    }

    ### Save engagementData ###
    if (!any(is.na(scenario$engagementData))){
        outName=sprintf("%s-engagementData.csv",saveLocation)
        utils::write.csv(scenario$engagementData,outName, row.names=FALSE)
    }

    ### Save platformInfo ###
    if (!any(is.na(scenario$platformInfo))){
        outName=sprintf("%s-platformInfo.csv",saveLocation)
        utils:: write.csv(scenario$platformInfo,outName, row.names=FALSE)
    }

    ### Save targetOwnShipDistance ###
    if (!any(is.na(scenario$targetOwnShipDistance))){
        outName=sprintf("%s-targetOwnShipDistance.csv",saveLocation)
        utils::write.csv(scenario$targetOwnShipDistance,outName, row.names=FALSE)
    }

    ### Save trackOwnShipDistance ###
    if (!any(is.na(scenario$trackOwnShipDistance))){
        outName=sprintf("%s-trackOwnShipDistance.csv",saveLocation)
        utils::write.csv(scenario$trackOwnShipDistance,outName, row.names=FALSE)
    }

    ### Save targetTrackDistance ###
    if (!any(is.na(scenario$targetTrackDistance))){
        outName=sprintf("%s-targetTrackDistance.csv",saveLocation)
        utils::write.csv(scenario$targetTrackDistance,outName, row.names=FALSE)
    }


}
