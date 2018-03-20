#' @title Get named colors for plotting (plotly)
#'
#' @description Internal function used by plotly functions.  Extracts the colors that should be used in the plot from platformInfo (contained in the scenario).  Alternatively, you can set useDefaultcolors = TRUE.
#''
#'
#' @param scenario must contain either or both: targetTruth, ownShipTruth
#' @param idList list of truthIDs or trackNums
#' @param defaultColorList list of default colors - must be as long as idList and in the same order (if you care)!
#' @param useDefaultColors if true, this function uses default color scheme (rather than colors in platformInfo). This has no effect if platformInfo is not present in the scenario (b/c we have to use default colors anyway)  (default=FALSE)
#'
#' @return plotly object
#'
#'

get_named_colors = function(scenario, idList, defaultColorList, useDefaultColors=FALSE) {
    #make dataframe with one row for each target

    truthColors=data.frame(truthID=idList) %>% mutate(truthID = as.character(truthID))

    if (!is.data.frame(scenario$platformInfo) || (useDefaultColors==TRUE) ){ #if we're using default colors
        truthColors = truthColors %>% mutate(targetColor=defaultColorList)

    } else { #otherwise, add in the platformInfo

        #force truthID to characters for joining
        localPlatInfo=scenario$platformInfo %>%
            mutate(truthID = as.character(truthID)) %>%
            select(-platformIcon, -platformType) #remove unnecessary stuff

        truthColors=left_join(truthColors,localPlatInfo, by="truthID") %>%
            rename(targetColor=trackColor)%>%
            mutate(targetColor = as.character(targetColor))

        #fill in any blanks with the default platform colors
        truthColors$targetColor[is.na(truthColors$targetColor)]=defaultColorList[is.na(truthColors$targetColor)]

    }
    # make the named list
    colorNames=stats::setNames(truthColors$targetColor,as.factor(truthColors$truthID))
    return(colorNames)

}

