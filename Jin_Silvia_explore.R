#########################
## Silvia Jin
## MATH 510 HW 07
## Jin_Silvia_explore.R
#########################


## Use class function

# function: explore
# Input: dataframe,
#        plotswitch, off, on, or grid
#        threshold,
#        bins,
# Output:
explore <- function(dataframe, plotswitch, threshold, bins){
  result01 <- freqTable(dataframe)
  result02a <- sumTable(dataframe)
  result02b <- rSquares(dataframe)
  result02c <- pearCoefs(dataframe,threshold)
  
  results <- list(result01,result02a,result02b,result02c)
  return (results)
}

# funcion: freqTable takes in a dataframe
# and returns a frequency table for every categorical and logical variable.
# Input: dataframe 
# Output: a frequency table.
freqTable <- function(dataframe){
  catData <- dataframe[sapply(dataframe,is.factor)]
  result <- c()
  l <- ncol(catData)
  for (i in 1:l){
    result <- c(result,table(catData[i]))
  }
  return(result)
}


# function: sumTable
# Input: numVar, a vector of numerical variable
# Output: a summary statistics table for the variable
sumTable <- function(dataframe){
  numericData <- dataframe[sapply(dataframe,is.numeric)]
  result <- c()
  l <- ncol(numericData)
  for (i in 1:l){
    result <- c(result,summary(numericData[i]))
  }
  return(result)
}


# function: rSquare
# Input: varName1, first numerical variable name
#        varName2, second numerical variable name
#        dataframe, the data that we are using
# Output: a number, which is the r-square value from the linear regression model
rSquare <- function(col1,col2,dataframe){
  fit <- lm(dataframe[[col1]] ~ dataframe[[col2]])
  return(summary(fit)$r.square)
}

# function: rSquare
# Input：
# Output:
rSquares <- function(dataframe){
  numericData <- dataframe[sapply(dataframe,is.numeric)] # select the numeric cols and store into a new frame
  numericNames <- colnames(numericData)# extract the numeric col names
  pairNames <- c() # create a null vector to store the names later
  pairRSquares <- c() # create a null vector to store the R squares later
  l <- ncol(numericData) # store the number of cols, to be used in the for loop
  for (i in 1:(l-1)){ # looking at each numeric col
    for (j in (i+1):l){ # compare each numeric col with each numeric col after it (no repetation)
      pairNames <- c(pairNames,paste(numericNames[i],numericNames[j],sep='-')) # create the name of the pair and store it
      pairRSquares <- c(pairRSquares,rSquare(i,j,dataframe)) 
    }
  }
  result <- data.frame(pairNames,pairRSquares)
  colnames(result) <- c("Variable Pairs","R-Square")
  return (result) # return the newly generated data as a data.frame
}

pearCoefs <- function(dataframe,threshold){
  #' This function accepts a data frame as parameter and returns a data frame that 
  #' contains each pair of column names in the first column in a single string
  #' seperated by a '-', and their corresponding Pearson Correlation Coefficient 
  #' in the second column.
  #' @param dataframe a data frame
  #' @param threshold a positive number, which the absolute value of the correlation should be greater than
  #' @return a data frame as described above
  numericData <- dataframe[sapply(dataframe,is.numeric)] # select the numeric cols and store into a new frame
  numericNames <- colnames(numericData)# extract the numeric col names
  pairNames <- c() # create a null vector to store the names later
  pairCoeffs <- c() # create a null vector to store the coeffs later
  l <- ncol(numericData) # store the number of cols, to be used in the for loop
  for (i in 1:(l-1)){ # looking at each numeric col
    for (j in (i+1):l){# compare each numeric col with each numeric col after it (no repetation)
      corcoef <- cor(numericData[i],numericData[j],method="pearson")
      if (abs(corcoef)>threshold){ # check if the absolute value of the calculated correlation is greater than the threshold
        pairNames <- c(pairNames,paste(numericNames[i],numericNames[j],sep='-')) # create the name of the pair and store it
        pairCoeffs <- c(pairCoeffs,corcoef) # store the correlation coefficient
      }
    }
  }
  result <- data.frame(pairNames,pairCoeffs)
  colnames(result) <- c("Variable Pairs","Pearson Exceeds Threshold")
  return (result) # return the newly generated data as a data.frame
}

# 
plotOn <- function(numVar,bin){
  
}

#
plotGrid <- function(numVar,bin){
  
}