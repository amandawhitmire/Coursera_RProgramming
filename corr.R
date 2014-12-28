# # # # # # # # # # # # # # # # # # # # # 
# takes a directory of data files and a threshold for complete 
# cases and calculates the correlation between sulfate and 
# nitrate for monitor locations where the number of completely 
# observed cases (on all variables) is greater than the threshold. 
# The function should return a vector of correlations for the 
# monitors that meet the threshold requirement. If no monitors 
# meet the threshold requirement, then the function should return 
# a numeric vector of length 0. A prototype of this function follows
#
# This is an assignment for the JHU/Coursera R Programming course
# # # # # # # # # # # # # # # # # # # # # 

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
  files <- dir(directory) # index all of the files in the directory
  completeObs <- complete("specdata")
  loop <- length(files)
  data <- numeric(0)
  
  for(i in 1:loop) { # loop through all monitors
    if(threshold < completeObs[i,2]){ # is threshold < complete cases?
      fnam <- paste(directory,"/",files[i], sep = "")
      foo <- read.table(fnam,sep=",",header=TRUE) # read in all data
      correl <- cor(foo[,2],foo[,3], use = "complete")
      data <- c(data,correl)
} else {
      0
    } #if loop
  } # for loop
data
} # function