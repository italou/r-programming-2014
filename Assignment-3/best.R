## This is part of the week 4 / prog assignment 3 for the Johns Hopkins
## / Coursera course "R Programming" (https://class.coursera.org/rprog-016).
##
## Assignment:
## Write a function called best that take two arguments: the 2-character
## abbreviated name of a state and an outcome name. The function reads the
## outcome-of-care-measures.csv file and returns a character vector with the
## name of the hospital that has the best (i.e. lowest) 30-day mortality for
## the specified outcome in that state.
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

best <- function(state = "TX", outcome = "heart attack") {

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
    
    # return best hospital (name)
    data[[1,1]]
}