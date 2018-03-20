#' @title Get distance info between specified targets at specified times
#'
#' @description Get distance info between specified targets at specified times.  Users input a scenario with truth data (target and/or sensor system truth positions), a list of which targets to calculate distance between, and a list of times at which the user wants these distances.
#'
#' This function may be useful if, for example, a user wanted to know how far apart all the targets were when a shot was fired at time t.  It can also be useful in the case where a user wants to know how far all targets were from Target A in the time leading up to an event around Target A.
#'
#'
#'
#' @param scenario names list as created by create_scenario.  Must contain either or both: targetTruth, ownShipTruth.  Between these two data frames there must be a minimum of two targets to have enough targets to calculate a distance between them.
#' @param timeList either a single number or list of numbers - these are the times for which you want data
#' @param ownShipList either a single name or a list of names - these are the reference platforms to measure from
#' @param targetList either a single name or a list of names - these are the targets to measure
#' @param windowScale (default=20) size of window that scenarioMaker will use for interpolation - two points must exist within windowScale/2*median truth gap  of your specified time for interpolation. The larger this value, the greater the risk of interpolating through very large truth data gaps
#'
#' @return A dataframe containing the ranges and bearings from the sensor system to every target at each time the position of the sensor system was measured. The output columns are a limited version of target_track_distance() and includes:
#' \itemize{
#'   \item{time: time of the measurement of ownship position}
#'   \item{ownShipTruthID: unique identifier for the reference platform}
#'   \item{ownShipLon: longitude of ownship at this time}
#'   \item{ownShipLat: latitude of ownship at this time}
#'   \item{ownShipAlt: altitude of ownship at this time}
#'   \item{slantRange: range from ownship to the target (straight-line distance)}
#'   \item{groundRange: range from ownship to the target (everyone's altitude is zeroed out)}
#'   \item{targetAspect: the target aspect (as seen from ownship) at this time}
#'   \item{trueBearingToTarget: true bearing (azimuth) to the target at this time}
#'   \item{relBearingToTarget: relative bearing (azimuth) to the target at this time}
#'   \item{targetTruthID: unique identifier for the target}
#'   \item{targetLon: target longitude at this time}
#'   \item{targetLat: target latitude at this time}
#'   \item{targetAlt: target altitude at this time}
#'   \item{targetHeading: target heading at this time}
#'   }
#'
#'
#' @export
#'

#' @importFrom foreach %do%
#'
#' @examples
#' distance_between(
#'  scenario=example1_scenario,
#'  timeList=c(34,75),
#'  ownShipList = c("A","B","C"),
#'  targetList = c("A","B","C","D","E"))


distance_between = function(scenario,timeList,ownShipList, targetList, windowScale=20){

    truthGaps=summarize_truth_gaps(scenario) %>% select(truthID, medianInterval) %>%
        mutate(windowSize=windowScale*medianInterval*0.5)


    combinedPositions=combine_target_ownship_truth(scenario) %>%
        left_join(truthGaps,by="truthID")

    combinations=expand.grid(getTime=timeList,ownShipTruthID=ownShipList)


    ################################################################
    ### let's make sure all ownShips and targets actually exist! ###
    ################################################################

    combinedIDList=c(ownShipList, targetList)
    missingIDs=setdiff(combinedIDList,unique(combinedPositions$truthID))
    if (length(missingIDs)>0){
        invalidIDs=paste(unlist(missingIDs),collapse=", ")

        validIDs=unique(combinedPositions$truthID) #figure out who WAS a valid choice so we can help the user out
        validIDsString=paste(unlist(validIDs),collapse=", ")

        s=sprintf("Your ownShip/target(s) '%s' do not exist ANYWHERE in the scenario. These are the truthIDs with associated truth data in the scenario: '%s'",invalidIDs,validIDsString)
        stop(s)
    }






    finalList=foreach::foreach(myRow = iterators::iter(combinations,by='row'),  .packages=c("dplyr")) %do% {
        #step through each time/ownShip combination, but always do ALL targets

        #remove the current ownShip from the list of targets (it may or may not be there)
        localTargetList=targetList[targetList != as.character(myRow$ownShipTruthID)]


        ### pull out JUST the targets we want, and ONLY times within the appropriate window ###
        newTargetTruth=combinedPositions%>%
            filter(truthID %in% localTargetList)%>%
            group_by(truthID) %>%
            filter(time<myRow$getTime+windowSize,time>myRow$getTime-windowSize)

        ### if some, but not all of the targets are missing (b/c they have nothing in the window), then we'll handle that at the end ###

        ################################################
        ### Error handling for target truth problems ###
        ################################################

        ### If there is NO target data in the window we just grabbed ###
        if (nrow(newTargetTruth)==0){
            listOfTargets=paste(unlist(localTargetList),collapse=", ")

            s=sprintf("None of your targets (%s) exist near time: %f\nPresumably your requested time is much too early, much too late, or in the middle of a truth data gap, so we'll skip this time request and move on to the next in the list. You can change the interpolation window size (default=20) with windowScale=X - beware: increasing it too much can result in interpolating truth data within very large gaps. You can investigate truth data gaps with plot_truth_gaps(scenario), summarize_truth_gaps(scenario), and summarize_time_exists(scenario).",listOfTargets,myRow$getTime)
            warning(s)

            ### there is no valid data to return, so let's insert a fake row for EACH target and abort this loop iteration ###
            fakeReturn=data.frame(time=myRow$getTime, ownShipTruthID = myRow$ownShipTruthID, targetTruthID = localTargetList) %>%
                mutate(targetTruthID = as.character(targetTruthID),
                       ownShipTruthID = as.character(ownShipTruthID))

            return(fakeReturn)
        }

        ### Even if there IS target data in the window, we still need enough points for good interpolation, so check that ###
        pointShortage=newTargetTruth %>% group_by(truthID) %>% summarize(numPoints = n()) %>% filter(numPoints<5)
        if (nrow(pointShortage)>0){
            invalidTargets=paste(unlist(pointShortage$truthID),collapse=", ")

            s=sprintf("The following target(s) have too few points in the window around time %f for proper interpolation: %s\nThis could cause problems, so we'll skip this time request and move on to the next in the list. You can change the interpolation window size (default=20) with windowScale=X - beware: increasing it too much can result in interpolating truth data within very large gaps. You can investigate truth data gaps with plot_truth_gaps(scenario), summarize_truth_gaps(scenario), and summarize_time_exists(scenario).",myRow$getTime,invalidTargets)
            warning(s)

            ### so, a subset of targets cannot be interpolated - let's pull them out of newTargetTruth ###
            ### the user will also get another warning at the end about this ###
            newTargetTruth = newTargetTruth %>% filter(truthID != pointShortage$truthID)
        }








        ### pull out JUST the ownship, and ONLY the times we want within the appropriate window
        newOwnShipTruth=combinedPositions%>%
            filter(truthID==as.character(myRow$ownShipTruthID))%>%
            filter(time<myRow$getTime+windowSize,time>myRow$getTime-windowSize)


        #################################################
        ### Error handling for ownShip truth problems ###
        #################################################


        if (nrow(newOwnShipTruth)==0){
            s=sprintf("Your chosen ownShip '%s' does not exist near the time: %f\nPresumably your requested time is much too early, much too late, or in the middle of a truth data gap, so we'll skip this time request and move on to the next in the list.\nYou can change the interpolation window size (default=20) with windowScale=X - beware: increasing it too much can result in interpolating truth data within very large gaps. You can investigate truth data gaps with plot_truth_gaps(scenario), summarize_truth_gaps(scenario), and summarize_time_exists(scenario).",myRow$ownShipTruthID,myRow$getTime)
            warning(s)

            ### there is no valid data to return, so let's insert a fake row for EACH target and abort this loop iteration ###
            fakeReturn=data.frame(time=myRow$getTime, ownShipTruthID = myRow$ownShipTruthID, targetTruthID = localTargetList)%>%
                mutate(targetTruthID = as.character(targetTruthID),
                       ownShipTruthID = as.character(ownShipTruthID))

            return(fakeReturn)
        }

        ### Even if there IS target data in the window, we still need enough points for good interpolation, so check that ###
        if (nrow(newOwnShipTruth)<5){
            s=sprintf("Your chosen ownShip '%s' has too few points in the window around time %f for proper interpolation\nThis could cause problems, so we'll skip this time request and move on to the next in the list. You can change the interpolation window size (default=20) with windowScale=X - beware: increasing it too much can result in interpolating truth data within very large gaps. You can investigate truth data gaps with plot_truth_gaps(scenario), summarize_truth_gaps(scenario), and summarize_time_exists(scenario).",myRow$ownShipTruthID,myRow$getTime)
            warning(s)


            ### there is no valid data to return, so let's insert a fake row for EACH target and abort this loop iteration ###
            fakeReturn=data.frame(time=myRow$getTime, ownShipTruthID = myRow$ownShipTruthID, targetTruthID = localTargetList)%>%
                mutate(targetTruthID = as.character(targetTruthID),
                       ownShipTruthID = as.character(ownShipTruthID))
            return(fakeReturn)
        }

        #######################################################################################################################
        ### Assuming we've made it this far, we're confident that we can interpolate ownShip and target to the time we want ###
        #######################################################################################################################

        ### interpolate ownShip to the getTime ###
        interpolatedOwnShipPosition <- interp_ellipse(newOwnShipTruth, myRow$getTime) #interpolate lat/lon/alt to a single time
        interpolatedOwnShipHeading=interp_heading(newOwnShipTruth$time,newOwnShipTruth$heading,myRow$getTime) #interpolate heading

        interpolatedOwnShip=interpolatedOwnShipPosition %>% #combine everything into a new truth dataframe
            mutate(heading=interpolatedOwnShipHeading,
                   truthID = myRow$ownShipTruthID)



        ### interpolate targets and get distance to ownship ###
        tempList=list()
        i=1
        for (target in (unique(newTargetTruth$truthID))) { #go through each target

            thisTargetTruth=filter(newTargetTruth,truthID==target) #pull out data for this particular target

            interpolatedTargetPosition <- interp_ellipse(thisTargetTruth, myRow$getTime) #interpolate lat/lon/alt to a single time
            interpolatedTargetHeading=interp_heading(thisTargetTruth$time,thisTargetTruth$heading,myRow$getTime) #interpolate heading

            interpolatedTargetTruth=interpolatedTargetPosition %>% #combine everything into a new truth dataframe
                mutate(heading=interpolatedTargetHeading,
                       truthID = target)

            tempList[[i]]=get_relative_distance(refPlatTruth=interpolatedOwnShip, targetTruth=interpolatedTargetTruth) %>%
                mutate(targetTruthID = as.character(targetTruthID), ### Convert targetTruthID and targetOwnShipID into characters for now to avoid joining issues later in this function - will fix outside the loop ###
                       ownShipTruthID = as.character(ownShipTruthID))

            i=i+1

        }

        ### bind each target into the list we're going to return
        newDist=bind_rows(tempList)







        ###########################################################################################
        ### figure out if any targets the user asked for are NOT in the data we're sending back ###
        ###########################################################################################

        missingTargets=setdiff(localTargetList,unique(newDist$targetTruthID))

        if (length(missingTargets) != 0 ){
            fakeReturn=data.frame(time=myRow$getTime, ownShipTruthID = myRow$ownShipTruthID, targetTruthID = missingTargets)%>%
                mutate(targetTruthID = as.character(targetTruthID),
                       ownShipTruthID = as.character(ownShipTruthID))


            newDist = bind_rows(newDist,fakeReturn)

            missingTargetList=paste(unlist(missingTargets),collapse=", ")
            s=sprintf("A subset of your targets (%s) do not exist near time: %f\nPresumably your requested time is much too early, much too late, or in the middle of a truth data gap, so we'll skip this time request and move on to the next in the list. You can change the interpolation window size (default=20) with windowScale=X - beware: increasing it too much can result in interpolating truth data within very large gaps. You can investigate truth data gaps with plot_truth_gaps(scenario), summarize_truth_gaps(scenario), and summarize_time_exists(scenario).",missingTargetList,myRow$getTime)
            warning(s)

        }

        ### we're done, return the dataframe that has the distances from this reference platform at this time to all targets
        return(newDist)
    }

    #bind each set of time/ownship/targets into the overall list for return
    finalList= bind_rows(finalList)



    ### Recast the truthIDs as factors (We had them as characters to avoid warnings about combining them earlier) ###
    finalList = finalList %>%
        mutate(targetTruthID = as.factor(targetTruthID), ### recast things as factors
               ownShipTruthID = as.factor(ownShipTruthID))

    return(finalList)

}
