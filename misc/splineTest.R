library(scenarioMaker)
library(tidyverse)
set.seed(12345)


# function to interpolate around a small window
interp_window <- function(dataSet, centerPoint, windowSize) {

    dataSet %>%
        filter(x > centerPoint - windowSize, x < centerPoint + windowSize) %>%
        summarize(npoints = n(),
                  interpValue = stats::spline(x, y, xout = centerPoint)$y)

}



### First attempt: interpolating a sine
x <- 0:20 + 0.5
y <- sin(x)
plot(x, y)

dataSet <- data.frame(x, y)

# run a bunch of windows at once
windowList <- list()
centerPoint = 10

for(windowSize in 1:12){
    windowList[[windowSize]] <- interp_window(dataSet, centerPoint, windowSize)
}

sinInterpolation <- do.call(rbind, windowList) %>%
    mutate(exact = sin(centerPoint),
           diff = exact - interpValue)

ggplot(sinInterpolation, aes(x = npoints, y = diff)) +
    geom_point() +
    geom_line() +
    labs(title = "Interpolating a sine")




### second attempt: interpolating a sine, but at a different point
x <- 0:20 + 0.5
y <- sin(x)
plot(x, y)

dataSet <- data.frame(x, y)

# run a bunch of windows at once
windowList <- list()
centerPoint = 11

for(windowSize in 1:12){
    windowList[[windowSize]] <- interp_window(dataSet, centerPoint, windowSize)
}

sinInterpolation <- do.call(rbind, windowList) %>%
    mutate(exact = sin(centerPoint),
           diff = exact - interpValue)

ggplot(sinInterpolation, aes(x = npoints, y = diff)) +
    geom_point() +
    geom_line() +
    labs(title = "Interpolating a sine")




### third attempt: interpolating a Gaussian at the peak
x <- 0:20 + 0.5
y <- exp(-(x - 10)^2 / 8)
plot(x, y)

dataSet <- data.frame(x, y)

# run a bunch of windows at once
windowList <- list()
centerPoint = 10

for(windowSize in 1:12){
    windowList[[windowSize]] <- interp_window(dataSet, centerPoint, windowSize)
}

gaussInterpolation <- do.call(rbind, windowList) %>%
    mutate(exact = exp(-(centerPoint - 10)^2 / 8),
           diff = exact - interpValue)

ggplot(gaussInterpolation, aes(x = npoints, y = diff)) +
    geom_point() +
    geom_line() +
    labs(title = "Interpolating at the peak of a gaussian")




### fourth attempt: interpolating a Gaussian mid-rise
x <- 0:20 + 0.5
y <- exp(-(x - 10)^2 / 8)
plot(x, y)

dataSet <- data.frame(x, y)

# run a bunch of windows at once
windowList <- list()
centerPoint = 8

for(windowSize in 1:12){
    windowList[[windowSize]] <- interp_window(dataSet, centerPoint, windowSize)
}

gaussInterpolation <- do.call(rbind, windowList) %>%
    mutate(exact = exp(-(centerPoint - 10)^2 / 8),
           diff = exact - interpValue)

ggplot(gaussInterpolation, aes(x = npoints, y = diff)) +
    geom_point() +
    geom_line() +
    labs(title = "Interpolating at the mid-rise of a gaussian")



### fifth attempt: interpolating a Gaussian at the shoulder
x <- 0:20 + 0.5
y <- exp(-(x - 10)^2 / 8)
plot(x, y)

dataSet <- data.frame(x, y)

# run a bunch of windows at once
windowList <- list()
centerPoint = 5

for(windowSize in 1:12){
    windowList[[windowSize]] <- interp_window(dataSet, centerPoint, windowSize)
}

gaussInterpolation <- do.call(rbind, windowList) %>%
    mutate(exact = exp(-(centerPoint - 10)^2 / 8),
           diff = exact - interpValue)

ggplot(gaussInterpolation, aes(x = npoints, y = diff)) +
    geom_point() +
    geom_line() +
    labs(title = "Interpolating at the shoulder (base) of a gaussian")
