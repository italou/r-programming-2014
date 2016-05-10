corr <- function(directory, threshold = 0, x  = "sulfate", y = "nitrate") {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    corrvector <- vector()
    
    # get folder file list
    filelist <- list.files(path = directory)
    
    # loop to check threshold condition and process file data
    for(i in 1:length(filelist)) {
        nobs <- complete(directory, i)[["nobs"]]
        
        # check threshold condition (compare complete cases)
        if(nobs > threshold) {
            # read data from file
            readdata <- read.csv(paste(directory, "/", filelist[i], sep = ""))
            # remove NA
            readdata <- readdata[complete.cases(readdata), ]
            # calculate correlation and adds it to vector
            corrvector <- c(corrvector, cor(readdata[[x]], readdata[[y]]))
        }
    }
    #return correlation(s) vector
    corrvector
}