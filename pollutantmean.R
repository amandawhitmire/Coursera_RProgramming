# # # # # # # # # # # # # # # # # # # # # 
# R function to that calculates the mean of a pollutant (sulfate or nitrate)
#  across a specified list of monitors (stations).
# The working directory contains 332 comma-separated-value (CSV) files 
# containing pollution monitoring data for fine particulate matter (PM) 
# air pollution at 332 locations in the United States. Each file contains 
# data from a single monitor and the ID number for each monitor is 
# contained in the file name. For example, data for monitor 200 is 
# contained in the file "200.csv". Each file contains three variables:
#   
#   Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#   sulfate: the level of sulfate PM in the air on that date 
#      (measured in micrograms per cubic meter)
#   nitrate: the level of nitrate PM in the air on that date 
#      (measured in micrograms per cubic meter)
# 
# This is an assignment for the JHU/Coursera R Programming course
# # # # # # # # # # # # # # # # # # # # # 
pollutantmean <- function(directory, pollutant, id = 1:332, removeNA = TRUE) {
  data <- numeric(0) # create empty vector for temp output
  files <- dir(directory) # index all of the files in the directory
  if(pollutant == "sulfate"){ # get which column to average
    x <- 2 # == sulfate
  } else {
    x <- 3 # == nitrate
  }
  for(i in min(id):max(id)) { # loop through selected monitors
    fnam <- paste(directory,"/",files[i], sep = "")
#   print(fnam)  # for debugging
    foo <- read.table(fnam,sep=",",header=TRUE) # read in all data
    foo <- foo[,x] # select data from pollutant of interest
    data <- c(data,foo) # add this column of data to previous monitor data
    rm(foo) # clear out the variable
  }
  avg <- mean(data, na.rm=removeNA) # calc mean w/o NA's
avg
}
