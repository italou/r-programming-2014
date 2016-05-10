## This is part of the week 4 / prog assignment 3 for the Johns Hopkins
## / Coursera course "R Programming" (https://class.coursera.org/rprog-016).
##
## Assignment:
## Write a function called rankhospital that takes three arguments: the
## 2-character abbreviated name of a state (state), an outcome (outcome), and
## the ranking of a hospital in that state for that outcome (num). The function
## reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the ranking specified by the num
## argument.
##
## Data used: Hospital Compare web site (http://hospitalcompare.hhs.gov)
##
## Author: Itamar M. B. Lourenço
##   Date: 2014-12-09


## Hospital.Name - col  2
##         State - col  7
##  Heart.Attack - col 11
## Heart.Failure - col 17
##     Pneumonia - col 23

rankhospital <- function(state = "TX", outcome = "heart attack", num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    # outcome list
    outcomes <- c("heart attack", "heart failure", "pneumonia")
 
    # check if outcome arg is valid
    if(!outcome %in% outcomes) {
        stop("invalid outcome")
    }
    
    # read data from file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # get state list from data
    states <- unique(data$State)
    
    # check if state is valid (on the list/data)
    if(!state %in% states) {
        stop("invalid state")
    }
    
    # set outcome column 
    if(outcome == "heart attack") {
        outcCol <- 11
    } else if(outcome == "heart failure") {
        outcCol <- 17
    } else {
        outcCol <- 23
    }
    
    # select state only hospitals
    hospitals <- data[,2][data[,"State"] == state]
    # get state hospitals outcome data
    counts <- data[,outcCol][data[,"State"] == state]
    # bind data
    data <- cbind(hospitals, counts)

    # just to keep memory usage low
    hospitals <- NULL
    counts <- NULL
    
    # removing NAs
    data <- data[complete.cases(as.numeric(data[,2])),]
    
    # sorting by lowest cases (and alphabetic name order)
    data <- data[order(as.numeric(data[,2]), data[,1], decreasing = FALSE),]
    
    # return ranked hospital (name)
    if(num == "best") {
        data[[1,1]]
    } else if(num == "worst") {
        data[[dim(data)[1],1]]
    } else if(num <= dim(data)[1]) {
        data[[num,1]]
    } else {
        NA
    }
}