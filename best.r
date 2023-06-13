#create a function
best<-function(state, outcome){
  data <- read.csv("/Users/.../Downloads/outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  
  ## make sure state and outcome are valid
best<- function(state, outcome)
{
  outcome1 <- read.csv("/Users/..../Downloads/outcome-of-care-measures.csv",
                       colClasses = "character")
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  minrow <- which(as.numeric(outcome2[ ,colnum]) == 
                     min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
  hospitals <- outcome2[minrow,2]
  hospitals <- sort(hospitals)
  return(hospitals[1])
    }
}
