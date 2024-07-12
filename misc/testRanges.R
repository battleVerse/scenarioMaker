myScenario=example1_scenario

thisTgt=myScenario$targetTruth %>% filter(truthID=="A") %>% slice(1) %>%mutate(truthID="J")

newTargetTruth=rbind(myScenario$targetTruth,thisTgt)

b=create_scenario(targetTruth = newTargetTruth,ownShipTruth = myScenario$ownShipTruth)


numPoints=newTargetTruth %>% group_by(truthID) %>% summarize(numPoints = n()) %>% filter(numPoints<1)


myScenario$targetTruth %>% group_by(truthID) %>% summarize(maxTime=max(time))

timeList=c(0,5,300,7,8,9,234)
targetList=c("A","B")
ownShipList=c("A","C")

q=distance_between(myScenario,timeList,ownShipList,targetList)

