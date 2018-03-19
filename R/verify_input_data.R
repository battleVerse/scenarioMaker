#' @title Checks the input format for create_scenario
#'
#' @description This function is called by create_scenario to check input dataframes to make sure they're compatible with scenarioMaker.
#'
#'  It checks for the following:
#'
#' * myData is a data frame
#'
#' * myData is not empty
#'
#' * myData has all of the columns in columnNames
#'
#' * the columns named in factorName are factors
#'
#'
#' @param myData object to be checked for correct format
#' @param columnNames column names that must be in myData
#' @param factorNames columns in myData that must be factors
#' @param stopOnFail Default = TRUE  When TRUE, function STOPS (bombs out) when dataframe fails a test and prints message about why. When FALSE, function simply returns FALSE.
#'
#' @return Bolean.  TRUE if input is in correct format, FALSE if input is in incorrect form and stopOnFail=FALSE. If stopOnFail=TRUE, then this will bomb out and return nothing
#'
#' @export

verify_input_data = function(myData, columnNames,factorNames, stopOnFail=TRUE){
    #not user facing


    ### Check input data frames to make sure they're acceptable ###
    if (!is.data.frame(myData)) { #is it a dataframe?
        errorMessage=sprintf("'%s' is not a data frame (and it needs to be!)",deparse(substitute(myData)))

        if (stopOnFail==TRUE) { #bomb out on error
            stop(errorMessage)
        } else { #return 'FALSE' on error
            return(FALSE)
        }
    }

    if (nrow(myData)==0){ #is it empty?
        errorMessage=sprintf("'%s' is empty - you need data!",deparse(substitute(myData)))

        if (stopOnFail==TRUE) { #bomb out on error
            stop(errorMessage)
        } else { #return 'FALSE' on error
            return(FALSE)
        }
    }

    result=columnNames %in% names(myData)

    if (any(!result)){ #does it have the right columns?
        missingColumns=paste(columnNames[!result],collapse=', ')
        errorMessage=sprintf("'%s' is missing the required column(s): %s",deparse(substitute(myData)),missingColumns)

        if (stopOnFail==TRUE) { #bomb out on error
            stop(errorMessage)
        } else { #return 'FALSE' on error
            return(FALSE)
        }
    }

    ### pull out the specified columns and see if they're factors (this step comes last for a reason - if it passed the column test, it won't crash here)
    result=myData %>% select(factorNames) %>% sapply(.,is.factor)

    if (any(!result)){ #are the specified columns actually factors?
        missingFactors=paste(factorNames[!result],collapse=', ')

        errorMessage=sprintf("The following column(s) in '%s' must be factors: '%s'",deparse(substitute(myData)),missingFactors)

        if (stopOnFail==TRUE) { #bomb out on error
            stop(errorMessage)
        } else { #return 'FALSE' on error
            return(FALSE)
        }

    }

    return(TRUE)
}
