rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data_outcome <- read.csv("/Users/.../Downloads/outcome-of-care-measures.csv", 
                             colClasses = "character")
    
    ## make sure that state is valid
    if (!is.element(state, outcome_data$State)) {
        stop("invalid state")
    }
    
    ## check that the outcome is valid
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, outcomes)) {
        stop("invalid outcome")
    }
    
    ## determine what data corresponds to the outcome
    if (outcome == "heart attack") {
        indexval <- 11
    } else if (outcome == "heart failure") {
        indexval <- 17
    } else if (outcome == "pneumonia") {
        indexval <- 23
    }    
    
    ## make a list with a data frame for each state
    state_l <- split(outcome_data, outcome_data$State)
    
    ## Generate data frame for state of interest only
    state_d <- state_l[[state]]
    
    ## Substitute row with %death rate of outcome to numeric values
    subrow <- gsub("Not Available", NA, state_d[, indexval])
    state_d[, indexval] <- as.numeric(subrow)
    
    ## Re-order the state data by hospital name first in ascending order then by death rate outcome in        ascending order
    orderedbyname <- state_d[order(state_d$Hospital.Name),]
    orderedstatedata <- orderedbyname[order(orderedbyname[,indexval], na.last = NA)
                                    ,]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    if (is.numeric(num) == TRUE) {
        rankedhospital <- orderedstatedata$Hospital.Name[num]
    } else if (num == "best") {
        rankedhospital <- orderedstatedata$Hospital.Name[1]
    } else if (num == "worst") {
        rankedhospital <- orderedstatedata$Hospital.Name[dim(orderedstatedata)
                                                         [1]] 
    }
    
    rankedhospital
        
}
