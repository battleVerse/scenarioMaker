#' @title Returns dataframe with target and ownship truth combined
#'
#' @description Creates Returns dataframe with target and ownship truth combined - is okay if one of the dataframes is NA
#'
#' @param scenario scenario
#'
#'
#' @return a position dataframe, which will include:
#' \itemize{
#' \item{time}
#' \item{lat}
#' \item{lon}
#' \item{alt}
#' \item{heading}
#' \item{truthID}
#' \item{extra columns either dataframe has}
#' }
#'
#'


combine_target_ownship_truth = function(scenario){

    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this function, the scenario must include target and/or ownship truth data.")
    }


    #if the scenario HAS target but NOT ownShip data
    if (is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        return(scenario$targetTruth)
    }

    #if the scenario HAS ownShip but NOT target data
    if (!is.data.frame(scenario$targetTruth) && is.data.frame(scenario$ownShipTruth)){
        return(scenario$ownShipTruth)
    }

    #if the scenario has BOTH ownShip AND target data
    if (is.data.frame(scenario$targetTruth) && is.data.frame(scenario$ownShipTruth)){

        #cast truthID as character for joining
        localTruth=scenario$targetTruth %>% mutate(truthID = as.character(truthID))
        localOwnShip=scenario$ownShipTruth %>% mutate(truthID = as.character(truthID))

        combinedData=bind_rows(localOwnShip, localTruth) %>% mutate(truthID = as.factor(truthID))

        return(combinedData)
    }
}
