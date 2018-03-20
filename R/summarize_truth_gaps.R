#' @title Summarize the gaps in truth/ownship data
#'
#' @description Table ennumerating gaps in truth data, measured in standard deviations
#'
#' @param scenario must contain either or both: targetTruth, ownShipTruth, which MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'}
#'
#'
#' @return Data frame containing a summary of how much time each target was tracked.  The data frame contains the collowing variables:
#'
#' \itemize{
#'  \item{"tgt" - Target name/identifier}
#'  \item{"# Intervals Above 2x Median" - The number of track update intervals longer than twice the median}
#'  \item{"# Intervals Above 5x Median" - The number of track update intervals longer than 5 times the median}
#'  \item{"# Intervals Above 10x Median" - The number of track update intervals longer than 10 times the median}
#'  \item{"Total # Intervals" - The total number of intervals for this target (this is just # truth points-1)}
#'  \item{"Std Dev" - This is the standard deviation of the truth data intervals (single number for all targets).}
#'  }
#'
#' @export
#'
#' @examples
#' summarize_truth_gaps(example1_scenario)


summarize_truth_gaps=function(scenario){
    #user facing


    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this summary function, the scenario must include target and/or ownship truth data.")
    }

    #combine the two (this is okay even if one is NA!)
    timeData=combine_target_ownship_truth(scenario)



    truthLagData=timeData %>%
        arrange(time)%>% #force it to be sorted by time
        group_by(truthID) %>%
        mutate(deltaTime=time-lag(time))


    #### Summarize by Target ####
    dropoutSummaryByTgt=truthLagData %>% group_by(truthID) %>%
        summarize(medianInterval=round(stats::median(deltaTime,na.rm=TRUE),3),
                  stdDev=round(stats::sd(deltaTime,na.rm=TRUE),3),
                  "# Intervals > 2x Median" = sum( deltaTime>(2*medianInterval) & deltaTime<(5*medianInterval), na.rm=TRUE ),
                  "# Intervals > 5x Median" = sum( deltaTime>(5*medianInterval) & deltaTime<(10*medianInterval), na.rm=TRUE ),
                  "# Intervals > 10x Median" = sum(deltaTime>(10*medianInterval), na.rm=TRUE),
                  "Total # Intervals"=n())


    return(dropoutSummaryByTgt)


}

