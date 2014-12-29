# # # # # # # # # # # # # # # # # # # # # 
# reads a directory full of files and reports the number of completely 
# observed cases in each data file. The function should return a data 
# frame where the first column is the name of the file and the second 
# column is the number of complete cases.  
#
# This is an assignment for the JHU/Coursera R Programming course
# # # # # # # # # # # # # # # # # # # # # 
complete <- function(directory, id = 1:332) {
  loop <- length(id)
  data <- numeric(0)
  idnum <- numeric(0)
  files <- dir(directory) # index all of the files in the directory
  for(i in 1:loop) { # loop through selected monitors
    fnam <- paste(directory,"/",files[id[i]], sep = "")
    foo <- read.table(fnam,sep=",",header=TRUE) # read in all data
    nobs <- complete.cases(foo) # get T/F vector of complete cases
    nobs <- sum(nobs) # sum complete case == True
    
    data <- c(data,nobs) # add this column of data to previous monitor data
    idnum <- c(idnum,id[i])
    rm(foo) # clear out the variable
  }
  
  df <- data.frame(idnum,data)
  colHeadings <- c('id','nobs')
  names(df) <- colHeadings
  df
}