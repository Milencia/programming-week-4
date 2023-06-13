rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("/Users/milenciasaintus/Downloads/outcome-of-care-measures.csv", colClasses = "character")

    ## Check that outcome is valid
    possibleOutcomes = c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, possibleOutcomes)) {
        stop("invalid outcome")
    }
    
    ## determine the data that corresponds to the outcome
    if (outcome == "heart attack") {
        indexval <- 11
    } else if (outcome == "heart failure") {
        indexval <- 17
    } else if (outcome == "pneumonia") {
        indexval <- 23
    } 
    
    ## make the row with the % of death rate of outcome to numeric values
    subrow <- gsub("Not Available", NA, outcome_data[, indexval])
    outcome_data[, indexval] <- as.numeric(subrow)
    

    ## make a list with a data frame for each state
    state_l <- split(outcome_data, outcome_data$State)
    
    results <- matrix(nrow = 54, ncol = 2)

    for (i in 1:length(state_l)) {

        state_d <- state_l[[i]]

        ## Re-order the state data by hospital name first in ascending order then by death rate outcome in        ascending order
        orderedbyname <- state_d[order(state_data$Hospital.Name),]
        ordereddata <- orderedbyname[order(orderedbyname[,indexval],
                                              na.last = NA),]

        curstate <- ordereddata$State[[1]]


        if (is.numeric(num) == TRUE) {
            newrow <- c(ordereddata$Hospital.Name[num], curstate)
        } else if (num == "best") {
            newrow <- c(ordereddata$Hospital.Name[[1]], curstate)
        } else if (num == "worst") {
            newrow <- c(ordereddata$Hospital.Name[dim(ordereddata)[1]], curstate)
        }

        results[i,] <- newrow

    }



    ## Return a data frame with the hospital names 
    
    namesr <- results[,2]
    df <- data.frame(results)
    names(df) <- c("hospital", "state")
    row.names(df) <- namesr
    
    df

}
