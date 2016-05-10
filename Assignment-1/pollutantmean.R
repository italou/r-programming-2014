pollutantmean <- function(directory, pollutant, id = 1:332, leadzeros = 3) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## 'leadzeros' is an integer value for the filenames
    
    # empty data frame
    pollutdata <- data.frame()
    
    # loop to read the monitor(s) data
    for(i in id) {
        filename <- as.character(i)
        filenmlead <- leadzeros - nchar(filename)
        
        # leadzing zeros on monitors files < 100
        while(filenmlead > 0) {
            filename <- paste("0", filename, sep = "")
            filenmlead <- filenmlead - 1
        }
        
        # reads the monitor data file and adds to data frame
        filename <- paste(directory, "/", filename, ".csv", sep = "")
        pollutdata <- rbind(pollutdata, read.csv(filename))
    }
    
    # Cleaning out NA
    pollutdata <- pollutdata[[pollutant]][complete.cases(pollutdata[[pollutant]])]
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(pollutdata)

} ## END pollutantmean