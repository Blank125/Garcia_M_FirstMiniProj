## First MiniProject in CMSC 197-2
## Michael John R. Garcia
## BS in Computer Science-IV

## Problem 1
##################################################################################################################################

##function polutantMean (since that's how its spelled in the paragraph) takes 3 arguments: directory(char vector, directory of the csv files),
##pollutant(char vector, choices are either sulfate or nitrate), and id(int vector, choices are the ids from the 332 files)
pollutantMean <- function(directory, pollutant, id = 1:332){
  
  generated_mean <- c()      #variable generated_mean is a vector used to store the means obtained from the files
  
  #for every monitor/element within the specified id, the path to the files is first obtained and placed in the path variable and the data obtained would be placed into mon_data. 
  #pollutant_data would then weed out unnecessary data in mon_data by only choosing the ones in the pollutant variable (which is either sulfate or nitrate). 
  #Finally, the generated_mean variable would be updated with every iteration until every monitor/element in id has been reached.
  #the updated generated_mean would always be combined with a vector of pollutant_data that always weeds out any NAs.
  
  for(mon in id){    
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", mon), ".csv", sep = "")
    
    mon_data <- read.csv(path)
    pollutant_data <- mon_data[pollutant]
    generated_mean <- c(generated_mean, pollutant_data[!is.na(pollutant_data)])
  }
  
  #At the end, all of means within generated_mean would also be averaged to get the mean
  mean(generated_mean)
}
  #sample code: after running the r script and letting pollutantMean be saved, simply call the function to show an output.
  #ex type in console: pollutantMean("specdata", "sulfate", 1:10), expected output: 4.064128
##################################################################################################################################


## Problem 2
##################################################################################################################################

##function complete takes in two arguments: directory(char vector, directory of the csv files) and id(int vector, choices are the ids from the 332 files)
complete <- function(directory, id = 1:332){
  
  data_frame <- data.frame(id=numeric(0), nobs=numeric(0)) #variable data_frame is a data frame used to display the number of complete cases obtained from the files
  
  #for every monitor/element within the specified id, the path to the files is first obtained and placed in the path variable and the data obtained would be placed into mon_data.
  #the number of complete cases is achieved through the following steps. Firstly, a variable named pollutant_data will store all the data obtained from mon_data and it will whittle it down to only non-NA sulfate data.
  #since the removed data only belonged to NA cases of sulfate data, pollutant_data is further weeded out to also include only non-NA nitrate data.
  #therefore all that would remain is the collection of both sulfate and nitrate non-NA cases/complete cases. The result would then be placed into the nobs variable.
  #Finally, the data_frame variable would be updated with every iteration until every monitor/element in id has been reached.
  #the first column in the data frame would be the name of the file (indicated by mon), and the second column would showcase the number of complete cases.
  
  for(mon in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", mon), ".csv", sep = "")
    
    mon_data <- read.csv(path)
    pollutant_data <- mon_data[(!is.na(mon_data$sulfate)), ]
    pollutant_data <- pollutant_data[(!is.na(pollutant_data$nitrate)), ]
    nobs <- nrow(pollutant_data)
    data_frame <- rbind(data_frame, data.frame(id=mon, nobs=nobs))
  }
  #data_frame would then be displayed (showing the output)
  data_frame
}
#sample code: after running the r script and letting complete be saved, simply call the function to show an output.
#ex type in console: complete("specdata", c(2,4,8,10,12)) 
#expected output:
#   id nobs
# 1  2 1041
# 2  4  474
# 3  8  192
# 4 10  148
# 5 12   96
##################################################################################################################################


##Problem 3
##################################################################################################################################

##function corr takes in two arguments: directory(char vector, directory of the csv files) and threshold(numeric vector, used in indicating the number of complete cases needed to compute the correlation, 0 is default)
corr <- function(directory, threshold = 0){
  
  correlation <- numeric(0)     #variable correlation would be a numeric vector of correlations (if it isn't updated at the end, it would showcase a numeric vector of length 0)

  #num_complete initially contains a data frame of numbers of complete cases in all files located within the directory.
  #after calling the complete function in order to contain all the numbers of complete cases, num_complete is updated and whittled down to include only numbers greater than or equal to the threshold.
  num_complete <- complete(directory)
  num_complete <- num_complete[num_complete$nobs>=threshold, ]
  
  #if there are numbers of complete cases greater than the threshold, then num_complete wouldn't be empty and the code can move further on.
  #however, if no monitors meet the threshold requirement, then the next block of code is skipped and the function will inevitably return a vector of length 0.
  
  #for every monitor/element within the specified id (based on/within num_complete), the path to the files is first obtained and placed in the path variable and the data obtained would be placed into mon_data.
  #similar to the previous function, a variable named pollutant_data will store all the data obtained from mon_data and it will whittle it down to only non-NA sulfate data.
  #since the removed data only belonged to NA cases of sulfate data, pollutant_data is further weeded out to also include only non-NA nitrate data.
  #Finally, the correlation variable would be updated with every iteration until every monitor/element in id has been reached.
  #the updated correlation variable would be a combination of correlation plus the result of using the cor() function between sulfate_data and nitrate_data.
  if(nrow(num_complete) > 0){
    for(mon in num_complete$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", mon), ".csv", sep = "")
      
      mon_data <- read.csv(path)
      
      pollutant_data <- mon_data[(!is.na(mon_data$sulfate)), ]
      pollutant_data <- pollutant_data[(!is.na(pollutant_data$nitrate)), ]
      
      sulfate_data <- pollutant_data["sulfate"]
      nitrate_data <- pollutant_data["nitrate"]
      correlation <- c(correlation, cor(sulfate_data, nitrate_data))
    }
  }
  #a numeric vector would be displayed(either a vector of correlations or a vector of length 0)
  correlation
}
#sample code: after running the r script and letting corr be saved, simply call the function to show an output.
#ex type in console: cr<-corr("specdata",150); head(cr); summary(cr)
#expected output(some of the output might differ slightly):
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.05147  0.09333  0.12401  0.26836  0.76313
##################################################################################################################################


##Problem 4
##################################################################################################################################

#this code is obtained from the pdf (used in plotting a histogram of the 30-day mortality rates from heart attacks)
#the only modification is that there are labels indicating the 30-day mortality rates from heart attacks
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "Deaths", main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack")

##################################################################################################################################