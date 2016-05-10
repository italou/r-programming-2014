complete <- function(directory, id = 1:332, leadzeros = 3) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## 'leadzeros' is an integer value for the filenames
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    
    # empty data frame
    casesdata <- data.frame()
    
    # loop to read the monitor(s) data
    for(i in id) {
        filename <- as.character(i)
        filenmlead <- leadzeros - nchar(filename)
        
        # leadzing zeros on monitors files < 100
        while(filenmlead > 0) {
            filename <- paste("0", filename, sep = "")
            filenmlead <- filenmlead - 1
        }
        
        # reads the monitor data file
        filename <- paste(directory, "/", filename, ".csv", sep = "")
        readata <- read.csv(filename)
        
        # adds data to data frame in form of id and total of complete obs cases
        casesdata <- rbind(casesdata, c(i, sum(complete.cases(readata))))
        
    }    
    
    # sets the name of the colums 
    colnames(casesdata) <- c("id", "nobs")
    
    # returns the data frame
    casesdata
}