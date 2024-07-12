library(scenarioMaker)
library(tidyverse)
library(purrr)
library(purrrlyr)
myScenario=example1_scenario

myScenario$engagementData[7,]$time=570.32142134
myScenario$engagementData[3,]$time=570.32142134


inputData=list(myTimes = myScenario$engagementData$time,
               myTargets = as.character(myScenario$engagementData$target),
               myOwnShip=as.character(myScenario$engagementData$source))


coolStuff=tryCatch({
    tmpData=pmap(inputData,
         function(myTimes, myOwnShip, myTargets)
         {distance_between(scenario = myScenario, #get distance_between for each specific engagement
                           timeList = myTimes,
                           ownShipList = myOwnShip,
                           targetList = myTargets)} %>%
             mutate(ownShipTruthID = as.character(ownShipTruthID), #convert factors to characters to make rbind smoother
                    targetTruthID = as.character(targetTruthID)
             )
    )
    myOut=bind_rows(tmpData) %>% #combine each level
        mutate(ownShipTruthID=as.factor(ownShipTruthID), #make the IDs factors again
               targetTruthID=as.factor(targetTruthID)) %>%
        rename( sourceLat= ownShipLat, sourceLon=ownShipLon, sourceAlt=ownShipAlt, sourceTruthID = ownShipTruthID) %>%
        select(-time) #this is redundant with the time already in engagementData


    finishedEngagements=cbind(myScenario$engagementData, myOut)


},
error = function(err){print("Oh no!")
    return(NA)}
    #print(err)
    #return(myScenario$engagementData)}
)

# tmpData[[14]]=NA
# myOut=do.call(rbind,tmpData) %>% #combine each level
#     mutate(ownShipTruthID=as.factor(ownShipTruthID), #make the IDs factors again
#            targetTruthID=as.factor(targetTruthID)) %>%
#     rename(gunTime=time, sourceLat= ownShipLat, sourceLon=ownShipLon, sourceAlt=ownShipAlt, sourceTruthID = ownShipTruthID)
#
#
# finishedEngagements=cbind(myScenario$engagementData, myOut)
#
#
#












# targetList=levels(myScenario$engagementData$target)
# ownShipList=levels(myScenario$engagementData$source)
# timeList=myScenario$engagementData$time
#
# a=distance_between(myScenario, targetList=targetList, ownShipList=ownShipList, timeList=timeList)
#
# a=Map(distance_between(myScenario, targetList, ownShipList, timeList),
#       #scenario=myScenario,
#       targetList = myScenario$engagementData$target,
#       ownShipList=myScenario$engagementData$source,
#       timeList=myScenario$engagementData$time)
#
# tmpList=list()
# i=1
#
# do.call(distance_between,myScenario)

#lapply(myScenario$engagementData, distance_between(.$time, .$source, .$target))

# Justace Place

# map_df(seq_len(nrow(myScenario$engagementData)), function(i) {
#     ed = myScenario$engagementData[i,]
#     distance_between(myScenario, targetList = ed$target, ownShipList = ed$source, timeList=ed$time)
# })
#
# myScenario$engagementData %>%
#     mutate(res = map(.[,c('target', 'source', 'time')], function(f) {
#         browser()
#         distance_between(myScenario, targetList=f$target, ownShipList=f$source, timeList=f$time)
#     }))
#
# myScenario$engagementData %>%
#     slice_rows() %>%
#     nest() %>%
#     map(function(ed) {
#         browser()
#         #        distance_between(myScenario, targetList = ed$target, ownShipList = ed$source, timeList=ed$time)
#     })
#
# # Justace End
#
#
# for (engagement in myScenario$engagementData){
#     a=2
#     tmpList[[i]]=distance_between(myScenario,targetList = engagement$target, ownShipList=engagement$source, timeList=engagement$time)
#
#     i=i+1
# }
# newInfo=do.call(rbind, tmpList)
#
# lOld=list(myTimes = myScenario$engagementData$time, myTargets = myScenario$engagementData$target, myOwnShip=myScenario$engagementData$source)
#
