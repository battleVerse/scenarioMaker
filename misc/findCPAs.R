library(scenarioMaker)
library(purrr)
library(tidyr)
library(ggplot2)

myFindpeaks <- function(vec,bw=1,x.coo=c(1:length(vec)))
{
    pos.x.max <- NULL
    pos.y.max <- NULL
    pos.x.min <- NULL
    pos.y.min <- NULL
    for(i in 1:(length(vec)-1)) 	{
        if((i+1+bw)>length(vec)){
            sup.stop <- length(vec)}else{sup.stop <- i+1+bw
            }
        if((i-bw)<1){inf.stop <- 1}else{inf.stop <- i-bw}
        subset.sup <- vec[(i+1):sup.stop]
        subset.inf <- vec[inf.stop:(i-1)]

        is.max   <- sum(subset.inf > vec[i]) == 0
        is.nomin <- sum(subset.sup > vec[i]) == 0

        no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
        no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)

        if(is.max & is.nomin){
            pos.x.max <- c(pos.x.max,x.coo[i])
            pos.y.max <- c(pos.y.max,vec[i])
        }
        if(no.max & no.nomin){
            pos.x.min <- c(pos.x.min,x.coo[i])
            pos.y.min <- c(pos.y.min,vec[i])
        }
    }

    result = NULL
    if(length(pos.x.max) > 0) result = rbind(result, data.frame(xVal=pos.x.max, yVal=pos.y.max, peak='Maximum'))
    if(length(pos.x.min) > 0) result = rbind(result, data.frame(xVal=pos.x.min, yVal=pos.y.min, peak='Minimum'))

    return(result)
}


myScenario=scenarioMaker::example1_scenario

# thisTgt=myScenario$targetOwnShipDistance %>% filter(truthID=="F")
# ranges=thisTgt$slantRange
# times=thisTgt$time
# r = findpeaks(ranges, x.coo = times)

x=seq(0,8*pi,.1)
cosList=cos(x)
sinList=sin(x)

fakeData1=data.frame(x=x,y=cosList,target="cos")
fakeData2=data.frame(x=x,y=sinList,target="sin")

fakeData=rbind(fakeData1,fakeData2)

q = fakeData %>%
    group_by(target) %>%
    nest(y, x) %>%
    mutate(peaks = map(data, function(i) {
        res = pracma::findpeaks(i$y,npeaks=5)[][1]
        return(res)
    })) %>%
    unnest(peaks)

pracma::findpeaks(fakeData1$y,npeaks=5)[,1]

r = myScenario$targetOwnShipDistance %>%
    group_by(targetTruthID) %>%
    nest(slantRange, time) %>%
    mutate(peaks = map(data, function(i) {
        res = myFindpeaks(i$slantRange,bw=2, x.coo = i$time)
        return(res)
    })) %>%
    unnest(peaks)

q = myScenario$targetOwnShipDistance %>%
    group_by(targetTruthID) %>%
    nest(slantRange, time) %>%
    mutate(peaks = map(data, function(i) {
        res = pracma::findpeaks(i$slantRange)
        return(res)
    })) %>%
    unnest(peaks)

thisTgt=myScenario$targetOwnShipDistance %>% filter(targetTruthID=="A")
peakList=pracma::findpeaks(thisTgt$slantRange,npeaks=5)

peakList[][1]


ggplot(r)+
    geom_point(aes(x=xVal,y=yVal,color=targetTruthID),size=5)+
    geom_line(data=myScenario$targetOwnShipDistance %>%group_by(targetTruthID),aes(x=time,y=slantRange,color=targetTruthID))
