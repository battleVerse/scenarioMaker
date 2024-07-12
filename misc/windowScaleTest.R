library(scenarioMaker)
library(ggplot2)
myScenario=scenarioMaker::example1_scenario

tmpList=list()
for (i in 5:40) {
    tmpList[[i-4]]=distance_between(myScenario,timeList=20.283, targetList="A", ownShipList = "ownShip", windowScale = i) %>% mutate(windowScale=i)
}

testData=do.call(rbind,tmpList)

ggplot(testData) +
    geom_line(aes(x=windowScale,y=targetLat)) +
    geom_point(aes(x=windowScale,y=targetLat)) +
    theme_bw()
