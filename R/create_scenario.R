#' @title Creates scenario
#'
#' @description Creates a scenario from the supplied data.  This is one of the first scenarioMaker functions that users will need and one of the most important as most of the functions take a scenario as input.  Users may supply as much of the raw data as they have, and create_scenario() will (by default) calculate as many of the target-track-ownship distances as possible.
#'
#' @param scenarioName name of scenario
#'
#' @param verbose (default=TRUE)
#'
#' @param targetTruth data frame containing all of the truth data for each target (likely from GPS or land-based radar systems). MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) target heading in degrees azimuth}
#'}
#'
#' @param sensorData data frame containing each sensor point for all of the tracks. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{trackNum: (factor) identifier for the track. We recommend numbers for each unique track returned by the sensor system}
#'  }
#'
#' @param ownShipTruth data frame containing all of the truth position of the sensor system (likely from GPS or land-based radar systems). This may be ownship if testing something on a ship or the lat/lon position of a stationary ststem. MUST have the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) sensor system heading in degrees azimuth}
#'}
#'
#' @param platformInfo display info for all platforms. Requires the following columns:
#' \itemize{
#'  \item{truthID: (factor) name or identifier for target}
#'  \item{platformIcon: (string) name of platform icon (must choose from valid SIMDIS options)}
#'  \item{platformType: (string) type of platform (e.g., ship) (must choose from valid SIMDIS options)}
#'  \item{trackColor: (string) color of the track (must be a valid SIMDIS choice)}
#'  }
#'
#'
#' @param engagementData Requires the following columns:
#' \itemize{
#'  \item{time: (double) time of the engagement}
#'  \item{source: (string) name of the platform doing the shooting}
#'  \item{target: (string) name of the target}
#'  \item{weapon: (string) name of the weapon}
#'  \item{kill: (double) 0 = no kill, 1 = kill}
#'  \item{color: (string) color of the line to be drawn (must be a valid SIMDIS choice)}
#'  }
#'
#' @param preCalcTargetTrackDist (default=TRUE) if false, will not pre-calculate targetTrackDistance (which can be big and slow)
#'
#' @return Scenario file, which will include:
#' \itemize{
#' \item{scenarioName}
#' \item{sensorData}
#' \item{targetTruth}
#' \item{ownShipTruth}
#' \item{engagementData: used for sandTable}
#' \item{platformInfo: used for SIMDIS export}
#' \item{targetTrackDistance}
#' \item{targetOwnShipDistance}
#' \item{trackOwnShipDistance}
#' }
#'
#' @examples
#'  myScenario = create_scenario(scenarioName="myScenario",targetTruth=example1_truthData,ownShipTruth=example1_ownShipData,sensorData=example1_sensorData,engagementData=example1_engagementData,platformInfo=example1_platformInfo,verbose=TRUE,preCalcTargetTrackDist=TRUE)
#'
#' @export


create_scenario = function(scenarioName="scenario",targetTruth=NA,ownShipTruth=NA,sensorData=NA,engagementData=NA,platformInfo=NA,verbose=TRUE,preCalcTargetTrackDist=TRUE){


    myScenario=list()

    myScenario[['scenarioName']]=scenarioName


    t=sprintf("\nCreating Scenario: \"%s\" - This May Take Several Minutes\n",myScenario$scenarioName)
    if(verbose){cat(t)}

    ###################
    ### Sensor Data ###
    ###################
    if (is.data.frame(sensorData)){

        #if the column is here, make sure it's a factor. if it's not here, verify_input_data will catch that
        if ('trackNum' %in% names(sensorData)){
            sensorData=mutate(sensorData, trackNum=as.factor(trackNum))
        }

        #check the dataframe for compliance with requirements
        requiredColumns=c("time","lat","lon","trackNum","alt")
        requiredFactors="trackNum"
        verify_input_data(sensorData,requiredColumns,requiredFactors,stopOnFail=TRUE)

        ### sort by time
        sensorData = sensorData %>% arrange(time)

        #assign the dataframe to the scenario
        myScenario[['sensorData']]=sensorData

        #print some information for the user
        startTime=as.POSIXct(min(myScenario$sensorData$time),tz="UTC",origin="1970-01-01")
        endTime=as.POSIXct(max(myScenario$sensorData$time),tz="UTC",origin="1970-01-01")
        numPoints=length(myScenario$sensorData$time)
        numTracks=length(unique(myScenario$sensorData$trackNum))

        t=sprintf("Loaded sensorData   Starts %s, Ends %s, contains %d points across %d tracks\n",startTime, endTime,numPoints,numTracks)
        if(verbose){cat(t)}

    } else {

        myScenario[['sensorData']]=NA
        if(verbose){cat("No sensorData Provided\n")}
    }


    #########################
    ### Target Truth Data ###
    #########################
    if (is.data.frame(targetTruth)){

        #if the column is here, make sure it's a factor. if it's not here, verify_input_data will catch that
        if ('truthID' %in% names(targetTruth)){
            targetTruth=mutate(targetTruth, truthID=as.factor(truthID))
        }

        #check the dataframe for compliance with requirements
        requiredColumns=c("time","lat","lon","truthID","heading","alt")
        requiredFactors="truthID"
        verify_input_data(targetTruth,requiredColumns,requiredFactors,stopOnFail=TRUE)


        ### Let's also make sure there aren't any single point truth targets ###
        numPoints=targetTruth %>% group_by(truthID) %>% summarize(numPoints = n()) %>% filter(numPoints<2)
        if (nrow(numPoints)>0){
            invalidTargets=paste(unlist(numPoints$truthID),collapse=", ")
            s=sprintf("The following target(s) have only a single point: %s\nThis will cause problems with interpolation! Either delete the target(s), or manually repeat the target(s) at a different time.",invalidTargets)
            stop(s)
        }

        ### sort by time
        targetTruth = targetTruth %>% arrange(time)

        #assign the dataframe to the scenario
        myScenario[['targetTruth']]=targetTruth

        #print some information for the user
        startTime=as.POSIXct(min(myScenario$targetTruth$time),tz="UTC",origin="1970-01-01")
        endTime=as.POSIXct(max(myScenario$targetTruth$time),tz="UTC",origin="1970-01-01")
        numPoints=length(myScenario$targetTruth$time)
        numTargets=length(unique(myScenario$targetTruth$truthID))

        t=sprintf("Loaded targetTruth  Starts %s, Ends %s, contains %d points across %d targets\n",startTime, endTime,numPoints,numTargets)
        if(verbose){cat(t)}

    } else {

        myScenario[['targetTruth']]=NA
        if(verbose){cat("No targetTruth Provided\n")}

    }


    ##########################
    ### OwnShip Truth Data ###
    ##########################
    if (is.data.frame(ownShipTruth)){

        #if the column is here, make sure it's a factor. if it's not here, verify_input_data will catch that
        if ('truthID' %in% names(ownShipTruth)){
            ownShipTruth=mutate(ownShipTruth, truthID=as.factor(truthID))
        }


        #check the dataframe for compliance with requirements
        requiredColumns=c("time","lat","lon","truthID","heading","alt")
        requiredFactors="truthID"
        verify_input_data(ownShipTruth,requiredColumns,requiredFactors,stopOnFail=TRUE)

        ### Let's make sure the user didn't accidentally include more than one platform in ownShipTruth ###
        if (length(unique(ownShipTruth$truthID)) > 1){
            listOfIDs=paste(unlist(unique(ownShipTruth$truthID)),collapse=", ")
            s=sprintf('You cannot have more than one unique truthID in your ownShipTruth data. Your input data contained the following truthIDs: %s',listOfIDs)
            stop(s)

        }


        ### sort by time
        ownShipTruth = ownShipTruth %>% arrange(time)

        #assign the dataframe to the scenario
        myScenario[['ownShipTruth']]=ownShipTruth

        #print some information for the user
        startTime=as.POSIXct(min(myScenario$ownShipTruth$time),tz="UTC",origin="1970-01-01")
        endTime=as.POSIXct(max(myScenario$ownShipTruth$time),tz="UTC",origin="1970-01-01")
        numPoints=length(myScenario$ownShipTruth$time)

        t=sprintf("Loaded ownShipTruth Starts %s, Ends %s, contains %d points\n",startTime, endTime, numPoints)
        if(verbose){cat(t)}

    } else {

        myScenario[['ownShipTruth']]=NA
        if(verbose){cat("No ownShipTruth Provided\n")}


    }


    ########################
    ### Truth Gap Checks ###
    ########################

    if (is.data.frame(myScenario$ownShipTruth) || is.data.frame(myScenario$targetTruth)){
        truthGaps=summarize_truth_gaps(myScenario)

        offenders=truthGaps %>%
            mutate(badGaps = `# Intervals > 5x Median` + `# Intervals > 10x Median`) %>% #combine the 5x and 10x columns - nonzero in either is a problem
            filter(badGaps>0) %>%
            select(truthID)

        if (nrow(offenders)>0){
            listOfOffenders=paste(unlist(offenders),collapse=", ")
            s=sprintf("The following platform(s) have truth gaps/dropouts longer than 10 times the median time between position updates: '%s'\n This could cause problems with interpolation.",listOfOffenders)
            warning(s)

            print("You have potentially problematic truth gaps/dropouts in your data!")
            print(truthGaps)
        }

    }


    #######################
    ### Engagement Data ###
    #######################
    if (is.data.frame(engagementData)){

        #if the column is here, make sure it's a factor. if it's not here, verify_input_data will catch that
        if (all(c('target','weapon','kill') %in% names(engagementData))){
            engagementData=mutate(engagementData, target=as.factor(target),weapon=as.factor(weapon),kill=as.factor(kill))
        }

        #check the dataframe for compliance with requirements
        requiredColumns=c("time","target","kill","weapon","source","color")
        requiredFactors=c("target","weapon","kill")
        verify_input_data(engagementData,requiredColumns,requiredFactors,stopOnFail=TRUE)


        ### sort by time
        engagementData = engagementData %>% arrange(time)

        ### if we have truth for everyone, we can give a lot more information about the engagements ###
        if (is.data.frame(targetTruth) && is.data.frame(ownShipTruth)){


            inputData=list(myTimes = engagementData$time,
                           myTargets = as.character(engagementData$target),
                           myOwnShip=as.character(engagementData$source))



            listOfEngagements=purrr::pmap(inputData,
                                   function(myTimes, myOwnShip, myTargets)
                                   {distance_between(scenario = myScenario, #get distance_between for each specific engagement
                                                     timeList = myTimes,
                                                     ownShipList = myOwnShip,
                                                     targetList = myTargets)} %>%
                                       mutate(ownShipTruthID = as.character(ownShipTruthID), #convert factors to characters to make bind_rows smoother
                                              targetTruthID = as.character(targetTruthID)
                                       )
            )

            engagementRangeInfo=bind_rows(listOfEngagements) %>% #combine each level
                mutate(ownShipTruthID=as.factor(ownShipTruthID), #make the IDs factors again
                       targetTruthID=as.factor(targetTruthID)) %>%
                rename( sourceLat= ownShipLat, #rename things to make them consistent with the engagementData terminology
                        sourceLon=ownShipLon,
                        sourceAlt=ownShipAlt,
                        sourceTruthID = ownShipTruthID) %>%
                select(-time) #this is redundant with the time already in engagementData


            engagementData=cbind(engagementData, engagementRangeInfo) #tack on the new info to the existing dataframe

            t=sprintf("Calculated ranges, bearings, and target aspects for all engagements (you can find this in scenarioName$engagementData)\n")
            if(verbose){cat(t)}
        }


        #assign the dataframe to the scenario
        myScenario[['engagementData']]=engagementData

        #print some information for the user
        numKills=sum(ifelse(myScenario$engagementData$kill=="1",1,0))
        numEngage=length(myScenario$engagementData$time)
        numWeapons=length(unique(myScenario$engagementData$weapon))

        t=sprintf("Loaded engagementData with %d weapons, %d engagements, and %d kills\n",numWeapons, numEngage, numKills)
        if(verbose){cat(t)}

    } else {

        myScenario[['engagementData']]=NA
        if(verbose){cat("No engagementData Provided\n")}


    }

    #####################
    ### Platform Info ###
    #####################
    if (is.data.frame(platformInfo)){

        #if the column is here, make sure it's a factor. if it's not here, verify_input_data will catch that
        if ('truthID' %in% names(platformInfo)){
            platformInfo=mutate(platformInfo, truthID=as.factor(truthID))
        }


        #check the dataframe for compliance with requirements
        requiredColumns=c("truthID","platformIcon","platformType","trackColor")
        requiredFactors="truthID"
        verify_input_data(platformInfo,requiredColumns,requiredFactors,stopOnFail=TRUE)


        #assign the dataframe to the scenario
        myScenario[['platformInfo']]=platformInfo

        numPlats=length(unique(myScenario$platformInfo$truthID))
        t=sprintf("Loaded platformInfo for %d platforms\n",numPlats)
        if(verbose){cat(t)}

    } else {

        myScenario[['platformInfo']]=NA
        if(verbose){cat("No platformInfo Provided\n")}


    }

    ###########################
    ### targetTrackDistance ###
    ###########################
    ### If we have targetTruth, ownShipTruth, and sensorData, we can calculate target_track_distance!
    if ( is.data.frame(myScenario$targetTruth) && is.data.frame(myScenario$sensorData) && is.data.frame(myScenario$ownShipTruth) && (preCalcTargetTrackDist) ) {

        if (verbose) {cat("Calculating target_track_distance\n")}

        #calculate and assign the dataframe to the scenario
        myScenario[['targetTrackDistance']]=target_track_distance(truthData = myScenario$targetTruth,
                                                                  sensorData = myScenario$sensorData,
                                                                  ownShipData = myScenario$ownShipTruth)
    } else {
        myScenario[['targetTrackDistance']]=NA
    }



    #############################
    ### targetOwnShipDistance ###
    #############################
    ### If we have targetTruth and ownShipTruth, we can calculate target_ownship_distance!
    if ( is.data.frame(myScenario$targetTruth) && is.data.frame(myScenario$ownShipTruth) ) {

        if (verbose) {cat("Calculating target_ownship_distance\n")}

        ### Check to make sure you have no overlaps in IDs between targets and ownShip!
        overlapList=intersect(unique(myScenario$targetTruth$truthID), unique(myScenario$ownShipTruth$truthID))

        if (length(overlapList) > 0){
            listOfIDs=paste(unlist(overlapList),collapse=", ")
            s=sprintf("You cannot have the same truthID for your ownShip and one of your targets. The culprit is: '%s'",listOfIDs)
            stop(s)
        }

        #calculate and assign the dataframe to the scenario
        myScenario[['targetOwnShipDistance']]=target_ownship_distance(truthData = myScenario$targetTruth,
                                                                      ownShipData = myScenario$ownShipTruth)
    } else {
        myScenario[['targetOwnShipDistance']]=NA
    }




    ############################
    ### trackOwnShipDistance ###
    ############################
    ### If we have sensorData and ownShipTruth, we can calculate track_ownship_distance!
    if ( is.data.frame(myScenario$sensorData) && is.data.frame(myScenario$ownShipTruth) ) {

        if (verbose) {cat("Calculating track_ownship_distance\n")}

        ### Check to make sure you have no overlaps in IDs between tracks and ownShip!
        overlapList=intersect(unique(myScenario$sensorData$trackNum), unique(myScenario$ownShipTruth$truthID))

        if (length(overlapList) > 0){
            listOfIDs=paste(unlist(overlapList),collapse=", ")
            s=sprintf("You cannot have the same truthID for your ownShip and one of your tracks. The culprit is: '%s'",listOfIDs)
            stop(s)
        }

        #calculate and assign the dataframe to the scenario
        myScenario[['trackOwnShipDistance']] = track_ownship_distance(sensorData = myScenario$sensorData,
                                                                      ownShipData = myScenario$ownShipTruth)
    } else {
        myScenario[['trackOwnShipDistance']] = NA
    }




    t=sprintf("\nFinished Creating Scenario: \"%s\"\n",myScenario$scenarioName)
    if(verbose){cat(t)}


    return(myScenario)


}


