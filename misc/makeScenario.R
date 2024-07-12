library(scenarioMaker)

truth=example1_truthData
ownShip=example1_ownShipData %>% mutate(speed=15)
sensor=example1_sensorData
platformInfo=example1_platformInfo
engagementData=example1_engagementData

example1_scenario=create_scenario(scenarioName="Example 1",targetTruth=truth,ownShipTruth=ownShip,sensorData=sensor, engagementData=engagementData, platformInfo=platformInfo)



distance_between(example1_scenario, timeList=c(50,200,250),ownShipList = c("ownShip","A","E"), targetList=c("A","B","C"))



testScenario=create_scenario(scenarioName="Test Scenario",
                             targetTruth=truth,
                             ownShipTruth=ownShip,
                             sensorData=sensor,
                             engagementData=engagementData,
                             platformInfo=platformInfo)



# ownShipInfo=data.frame(truthID="ownShip",platformIcon = 'uss_independance_lcs_2', platformType = "ship", trackColor = 'yellow')
#
# targetInfo=distinct(truth,truthID) %>%
#     mutate(platformIcon = "HSMST", platformType = "ship", trackColor = 'red')
#
# sensorInfo=sensor  %>% rename(truthID = trackNum) %>% mutate(heading=0) %>% distinct(sensor,truthID) %>%
#     mutate(platformIcon = "NTDSUnknownUnknown2D", platformType = "ship", trackColor = 'gray')
#
# platformInfo=rbind(targetInfo,ownShipInfo,sensorInfo)
### Create Gun Data ###
# engagementData = data.frame(time=c(180,185,193,270,275,280,285,250,255,265,300,315,330),
#                             source=c("ownShip","ownShip","ownShip","ownShip","ownShip","ownShip","ownShip","ownShip","ownShip","ownShip","B","B","B"),
#                             target=c("E","E","E","H","H","H","H","A","A","A","ownShip","ownShip","ownShip"),
#                             weapon=c("302","302","302","302","302","302","302","301","301","301","RPG","RPG","RPG"),
#                             color=c('green','green','green','green','green','green','green','magenta','magenta','magenta','white','white','white'),
#                             kill=c(0,0,1,0,0,0,1,0,0,1,0,0,0))
