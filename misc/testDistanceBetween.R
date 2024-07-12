library(scenarioMaker)

#truth=example1_truthData
ownShip=example1_ownShipData %>% mutate(speed=15)
sensor=example1_sensorData
platformInfo=example1_platformInfo
engagementData=example1_engagementData



truthDrops=nautilus::scenario3_truthData_dropout
dropoutScenario=create_scenario(scenarioName="Dropout 1",
                                targetTruth=truthDrops,
                                ownShipTruth=ownShip,
                                sensorData=sensor,
                                engagementData=engagementData,
                                platformInfo=platformInfo)

regularScenario=scenarioMaker::example1_scenario

## tgt G has dropout between 30 and 100
timeList=c(33,120)
ownShipList=c("G","ownShip")
targetList=c("G","H")


dListGood=distance_between(regularScenario,timeList = timeList, ownShipList=ownShipList, targetList = targetList)

### the 33 should trigger a warning about not having enough points for target G
dList=distance_between(dropoutScenario,timeList = timeList, ownShipList=ownShipList, targetList = targetList)




targetTimes=c(1,2,3,4,5,6,7)
ownShipTimes=c(1,2,3.5, 4.5, 6, 7)

setdiff(ownShipTimes,targetTimes)

xTimes=c(1,2,3,4)
xVals=c(1,2,3,2)
spline(xTimes, xVals, xout = c(0,2,3,5,100))$y
