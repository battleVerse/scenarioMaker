#' @title Summarize truth data for targets
#'
#' @description Table showing how long each target existed (according to truth data)
#'
#' @param scenario must contain targetTruth and/or ownShipTruth
#'
#' @return data frame containing a summary of how long each target existed (i.e., how much truth data exists for each target)
#'
#' \itemize{
#'  \item{"truthID": Name of target}
#'  \item{"startTime": First recorded time for each target}
#'  \item{"stopTime": Last recorded time for each target}
#'  \item{"timeExists": difference between stopTime and startTime (in seconds)}}
#'
#' @export
#'
#' @examples
#' summarize_time_exists(example1_scenario)




summarize_time_exists <- function(scenario){
    #user facing

    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this summary function, the scenario must include target and/or ownship truth data.")
    }

    #combine the two (this is okay even if one is NA!)
    truthData=combine_target_ownship_truth(scenario)
    #


    #truthData=scenario$targetTruth

    ### Figure first and last time that we have data for each target
    truthStartStop <- truthData %>%
        mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01")) %>%
        group_by(truthID) %>%
        summarise(stopTime = max(time), startTime = min(time)) %>%
        ungroup() %>%
        mutate(timeExists = difftime(stopTime, startTime, units = 'secs'))

    return(truthStartStop)
}


