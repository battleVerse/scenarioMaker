library(foreach)
library(scenarioMaker)
library(iterators)




distance_between = function(scenario,timeList,ownShipList, targetList){

    combinedPositions=rbind(myScenario$targetTruth,myScenario$ownShipTruth) %>%
        na.omit()

    combinations=expand.grid(getTime=timeList,ownShipTruthID=ownShipList)

    finalList=foreach::foreach(myRow = iter(combinations,by='row'), .combine="rbind", .packages=c("dplyr")) %do% {

        localTargetList=targetList[targetList != as.character(myRow$ownShipTruthID)]
        newTargetTruth=combinedPositions%>%filter(truthID %in% localTargetList)%>% group_by(truthID) %>% filter(time<myRow$getTime+5,time>myRow$getTime-5)
        newOwnShipTruth=combinedPositions%>%filter(truthID==as.character(myRow$ownShipTruthID))%>% filter(time<myRow$getTime+5,time>myRow$getTime-5)
        newDist=target_ownship_distance(truthData = newTargetTruth,
                                        ownShipData = newOwnShipTruth)

        newDist=newDist%>% group_by(truthID)%>% filter(truthID %in% as.character(localTargetList)) %>% slice(which.min(abs(time-myRow$getTime)))
        return(newDist)
    }

    return(finalList)

}


getTime=c(25,35,74)
targetTruthID=c("B","D")
ownShipTruthID=c("B","C")

myScenario=scenarioMaker::example1_scenario

manyList=distance_between(myScenario,timeList=c(34,75),ownShipList = c("A","B","C"),targetList = c("A","B","C","D","E"))

singleList=distance_between(myScenario,timeList=5,ownShipList = "C",targetList = "D")
