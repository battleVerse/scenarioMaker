
library(scenarioMaker)
library(stringr)


myScenario=scenarioMaker::example1_scenario
myScenario$scenarioName="testScenario"


outLocation="U:/"


### Check to see if user remembered trailing slash on file location. If not, add it for them ###
lastChar=str_sub(outLocation,-1,-1)
if (lastChar != "/"){
    outLocation=sprintf("%s/",outLocation)
}



### Make sure the scenario name doesn't have any forbidden characters ###
forbiddenCharacters=c("<",">",":","/","\\","|","?","*")
forbiddenMatches=intersect(strsplit(myScenario$scenarioName,"")[[1]],forbiddenCharacters)
if (length(forbiddenMatches)>0){
    listOfBad=paste(unlist(forbiddenMatches),collapse=", ")
    s=sprintf("Your scenario's name: '%s' contains the symbol(s): %s\nThese cannot be used as a file name in Windows. Either change the scenario's name (the 'scenarioName' property), or use the argument 'saveName' when calling this function.",myScenario$scenarioName,listOfBad)
    stop(s)
}

### Combine the output location with the scenario name ###
outLocation=sprintf("%s%s",outLocation,myScenario$scenarioName)


### Save targetTruth ###
if (!any(is.na(myScenario$targetTruth))){
    outName=sprintf("%s-targetTruth.csv",outLocation)
    write.csv(myScenario$targetTruth,outName, row.names=FALSE)
}

### Save ownShipTruth ###
if (!any(is.na(myScenario$ownShipTruth))){
    outName=sprintf("%s-ownShipTruth.csv",outLocation)
    write.csv(myScenario$ownShipTruth,outName, row.names=FALSE)
}

### Save sensorData ###
if (!any(is.na(myScenario$sensorData))){
    outName=sprintf("%s-sensorData.csv",outLocation)
    write.csv(myScenario$sensorData,outName, row.names=FALSE)
}

### Save engagementData ###
if (!any(is.na(myScenario$engagementData))){
    outName=sprintf("%s-engagementData.csv",outLocation)
    write.csv(myScenario$engagementData,outName, row.names=FALSE)
}

### Save platformInfo ###
if (!any(is.na(myScenario$platformInfo))){
    outName=sprintf("%s-platformInfo.csv",outLocation)
    write.csv(myScenario$platformInfo,outName, row.names=FALSE)
}

### Save targetOwnShipDistance ###
if (!any(is.na(myScenario$targetOwnShipDistance))){
    outName=sprintf("%s-targetOwnShipDistance.csv",outLocation)
    write.csv(myScenario$targetOwnShipDistance,outName, row.names=FALSE)
}
