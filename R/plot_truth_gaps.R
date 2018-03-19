#' @title Plot the gaps in truth data
#'
#' @description This figure plots the gaps in truth data by target. Takes 1 or 2 dataframes, intended to plot both truth and/or ownship data
#'
#' @param scenario must contain either or both: targetTruth, ownShipTruth
#'
#' @return ggplot object
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' plot_truth_gaps(example1_scenario)



# a few functions to help with plotting

#user facing


plot_truth_gaps = function(scenario) {

    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this plotting function, the scenario must include target and/or ownship truth data.")
    }

    ### Get gaps for plotting horizontal lines ###
    gapSummary=summarize_truth_gaps(scenario)

    #combine the two (this is okay even if one is NA!)
    timeData=combine_target_ownship_truth(scenario)


    ### Get interval deltas ###
    timeLags=timeData %>%
        arrange(time)%>% #force it to be sorted by time
        group_by(truthID) %>%
        mutate(deltaTime=time-lag(time)) %>%
        #na.omit() %>%
        ungroup()



    combinedData=left_join(timeLags,gapSummary,by="truthID") %>%
        mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01"))


    myPlot=ggplot(combinedData)+
        geom_point(aes(x=time,y=deltaTime),size=2)+
        geom_line(aes(x=time,y=deltaTime))+
        geom_hline(aes(yintercept=2*medianInterval,color="2x Median Interval"))+
        geom_hline(aes(yintercept=5*medianInterval,color="5x Median Interval"))+
        geom_hline(aes(yintercept=10*medianInterval,color="10x Median Interval"))+
        facet_wrap(~truthID)+
        scale_y_log10()+
        xlab("Time")+
        ylab("Gaps in Truth Data (s)")+
        ggtitle("Missing Truth Data by Target")+
        scale_color_discrete(guide = guide_legend(title = ""))+
        theme_bw()


    return(myPlot)
}
