## This is part of the week 4 / prog assignment 3 for the Johns Hopkins
## / Coursera course "R Programming" (https://class.coursera.org/rprog-016).
##
## Assignment:
## Write a function called rankall that takes two arguments: an outcome name
## (outcome) and a hospital ranking (num). The function reads the
## outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.
## For example the function call rankall("heart attack", "best") would return a
## data frame containing the names of the hospitals that are the best in their
## respective states for 30-day heart attack death rates. The function should
## return a value for every state (some may be NA). The first column in the data
## frame is named hospital, which contains the hospital name, and the second
## column is named state, which contains the 2-character abbreviation for the
## state name. Hospitals that do not have data on a particular outcome should be
## excluded from the set of hospitals when deciding the rankings.
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

rankall <- function(outcome = "heart attack", num = "best") {
    # outcome list
    outcomes <- c("heart attack", "heart failure", "pneumonia")
 
    # check if outcome arg is valid
    if(!outcome %in% outcomes) {
        stop("invalid outcome")
    }
    
    # read data from file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # keep only necessary data according the outcome
    if(outcome == "heart attack") {
        data <- data[,c(2,7,11)]
    } else if(outcome == "heart failure") {
        data <- data[,c(2,7,17)]
    } else {
        data <- data[,c(2,7,23)]
    }
    # just to give a "name" to outcome rate column
    names(data)[3] <- "Cases"
    # "convert" to numeric, for getting the NAs
    data[, 3] <- as.numeric(data[, 3])
    # remove NAs from data
    data = data[!is.na(data$Cases),]
    # split data into states
    splitdata <- split(data, data$State)
    
    ## function to rank hospitals in a state
    rank_hospitals <- function(hospdata, num) {
        # order/rank the hospitals by cases
        hospdata <- hospdata[order(hospdata$Cases, hospdata$Hospital.Name,
                                   na.last=NA),]
        
        # return ranked hospital
        if(num == "best") {
            hospdata$Hospital.Name[1]
        } else if(num == "worst") {
            hospdata$Hospital.Name[nrow(hospdata)]
        } else if(num <= nrow(hospdata)) {
            hospdata$Hospital.Name[num]
        } else {
            NA
        }
    }
    
    # apply rank function to data
    rankdata <- lapply(splitdata, rank_hospitals, num)
    
    # create data frame with ranked hospitals and return it
    data.frame(hospital = unlist(rankdata), state = names(rankdata),
               row.names = names(rankdata))
    
}