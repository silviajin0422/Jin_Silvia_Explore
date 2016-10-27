#########################
## Silvia Jin
## MATH 510 HW 07
## Jin_Silvia_explore.R
#########################


#########################################
### Part 1.
# funcion: freqTable 
# This is a helper function dealing with part 1 in the project.
# This is the function called directly in the explore() function.
# This fucntion takes in a dataframe and returns a frequency table 
# for every categorical and logical variable.
# Input: dataframe, the data that we are using
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

#########################################
### Part 2a.
# function: sumTable
# This is a helper function dealing with part 2a in the project.
# This is the function called directly in the explore() function.
# This fucntion takes in a dataframe and returns a summary statistics
# table for each numerical variable.
# Input: dataframe, the data that we are using
# Output: a summary statistics table for each numeric variable in the dataframe.
sumTable <- function(dataframe){
  numericData <- dataframe[sapply(dataframe,is.numeric)]
  result <- c()
  l <- ncol(numericData)
  for (i in 1:l){
    result <- c(result,summary(numericData[i]))
  }
  return(result)
}

#########################################
### Part 2b.
# function: rSqaure
# This is a helper function dealing with part 2a in the project.
# This function is not called in the explore() function.
# This is only a smaller helper function for rSquares() which is called in explore().
# This fucntion fits a model on two variables in a dataframe and returns the r square value.
# Input: col1, the column number of the first numerical variable name
#        col2, the column number of the second numerical variable name
#        dataframe, the data that we are using
# Output: a number, which is the r-square value from the linear regression model
rSquare <- function(col1,col2,dataframe){
  fit <- lm(dataframe[[col1]] ~ dataframe[[col2]])
  return(summary(fit)$r.square)
}

# function: rSquare
# This is a helper function dealing with part 2a in the project.
# This function is called derictly in the explore() function.
# This fucntion takes in a dataframe, fits a model on each pair of numeric
# variables, get the r square, and return all r squares in a dataframe.
# Input: dataframe, the data that we are using
# Output: a data frame that contains each pair of column names in the first column
#         (the column name is "Variable Pairs") and the associated r-square value in the 
#         second column (the column name is "R-Square")
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


#########################################
### Part 2c.
# function: pearCoefs
# This is a helper function dealing with part 2c in the project.
# This is the function called directly in the explore() function.
# This fucntion takes in a dataframe, a threshold and returns a data frame
# that contains each pair of column names in the first column (the column name
# is "Variable Pairs") and correlation coefficient (Pearson) for all coefficients
# whose absolute value is greater than the correlation threshold in the second column
# (the column name is "Pearson Exceeds Threshold").
# Input: dataframe, the data that we are using
#        threshold, a number which is the threshold
# Output: a data frame as described above
pearCoefs <- function(dataframe,threshold){
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

#########################################
### Part 3 and 4.

# function: pearCoefs
# This is a helper function dealing with part 3 in the project.
# This is the function called directly in the plotOn() function, which is called in explore().
# This fucntion takes in a dataframe, a threshold and returns a data frame
# that contains each pair of column names in the first column (the column name
# is "Variable Pairs") and correlation coefficient (Pearson) for all coefficients
# whose absolute value is greater than the correlation threshold in the second column
# (the column name is "Pearson Exceeds Threshold").
# Input: dataframe, the data that we are using
#        threshold, a number which is the threshold
# Output: a data frame as described above

# Input: dataframe, the data that we are using
plotNumericOn <- function(dataframe,col,bin){
  par(mfrow = c(1,2))
  xName <- names(dataframe)[col]
  colMean <- mean(dataframe[[col]])
  if (is.null(bin)){
  hist(dataframe[[col]],main = 'Frequency histogram',
       xlab = xName,ylab='Frequency',col = c('blue'))
  abline(v = colMean, col = 'red')
  hist(dataframe[[col]],main = 'Density histogram',
       freq = FALSE,
       xlab = xName,ylab='Density',col = c('blue'))
  abline(v = colMean, col = 'red')
  }
  else {
    hist(dataframe[[col]],main = 'Frequency histogram', breaks = bin,
         xlab = xName,ylab='Frequency',col = c('blue'))
    abline(v = colMean, col = 'red')
    hist(dataframe[[col]],main = 'Density histogram', breaks = bin,
         freq = FALSE,
         xlab = xName,ylab='Density',col = c('blue'))
    abline(v = colMean, col = 'red')
  }
}

# function: plotNumericGrid
# This is a helper function dealing with part 3 in the project.
# This is the function called directly in the plotGrid() function, which is called in explore().
# This fucntion takes in a numeric dataframe, a bin number and prints a graph
# containing subplots.
# Input: numericData, a data frame containing only numeric data
#        bin, a number, implying the bin
# Prints a graph with n subplots (n being the number of variables in the numericData)
plotNumericGrid <- function(numericData,bin){
  l <- ncol(numericData)
  m <- (l+1)%/%2
  par(mfrow = c(2,m))
  for (i in 1:l){
    xName <- names(numericData)[i]
    colMean <- mean(numericData[[i]])
    if (is.null(bin)){
      hist(numericData[[i]],main = NULL,
          xlab = xName,ylab='Frequency',col = c('blue'))
      abline(v = colMean, col = 'red')
    }
    else {
      hist(numericData[[i]],main = NULL, breaks = bin,
           xlab = xName,ylab='Frequency',col = c('blue'))
      abline(v = colMean, col = 'red')     
    }
  }
  par(mfrow = c(2,m))
  for (i in 1:l){
    xName <- names(numericData)[i]
    colMean <- mean(numericData[[i]])
    if (is.null(bin)){
      hist(numericData[[i]],main = NULL,
          freq = FALSE,
          xlab = xName,ylab='Density',col = c('blue'))
      abline(v = colMean, col = 'red')
    }
    else {
      hist(numericData[[i]],main = NULL,
           freq = FALSE, breaks = bin,
           xlab = xName,ylab='Density',col = c('blue'))
      abline(v = colMean, col = 'red')
    }
  }
}

# function: plotCat
# This is a helper function dealing with part 4 in the project.
# This is the function called directly in the plotOn() and plotGrid() functions, which are called in explore().
# This fucntion takes in a data frame, a column number and prints a histogram to the screen.
# Input: dataframe, the data that we are using
#        col, the column number of a categorical or binary variable in the data
# Prints a histogram of the categorical variable to the screen
plotCat <- function(dataframe,col){
  par(mfrow = c(1,1))
  xName <- names(dataframe)[col]
  title <- paste('Histgram for',xName)
  hist(diamonds[[7]],main = title,
       xlab = xName,col = c('gray'))
}

# function: plotOn
# This is a helper function dealing with part 3 and 4 in the project.
# This is the function called directly in explore().
# Input: dataframe, the data that we are using
#        bins, a vector of the bin numbers
# Plots a pair of blue histograms with a vertical red line at the mean (one using counts and the other density)
# for every numerical variable at each number of bins integer specified in the bin vector parameter.
# Plots a gray bar graph for every categorical and binary variable.
plotOn <- function(dataframe,bins){
  numericData <- dataframe[sapply(dataframe,is.numeric)] # select the numeric cols and store into a new frame
  catData <- dataframe[sapply(dataframe,is.factor)]
  if (is.null(bins)){
    for (col in 1:ncol(numericData)){
        plotNumericOn(numericData,col,bin=NULL)
    }
  }
  else {
    for (col in 1:ncol(numericData)){
      for (bin in bins){
        plotNumericOn(numericData,col,bin)
      }
    }
  }
  for (col in 1:ncol(catData)){
    plotCat(catData,col)
  }
}

# function: plotGrid
# This is a helper function dealing with part 3 and 4 in the project.
# This is the function called directly in explore().
# Input: dataframe, the data that we are using
#        bins, a vector of the bin numbers
# Plots a blue histogram with a vertical red line at the mean (one using counts and the other density)
# for every numerical variable at each number of bins integer specified in the bin vector parameter. There
# is a grid for each count-bin combination and a separate grid for each density-bin size combination.
# Plots a gray bar graph for every categorical and binary variable.
plotGrid <- function(dataframe,bins){
  numericData <- dataframe[sapply(dataframe,is.numeric)] # select the numeric cols and store into a new frame
  catData <- dataframe[sapply(dataframe,is.factor)]
  if (is.null(bins)){
    plotNumericGrid(numericData,bin=NULL)
  }
  else {
    for (bin in bins){
      plotNumericGrid(numericData,bin)
    }
  }
  for (col in 1:ncol(catData)){
    plotCat(catData,col)
  }
}


#########################################
### main function #######################
#########################################

# function: explore
# Input: dataframe, the data that we are using
#        plotswitch, off, on, or grid
#        threshold, a number bettwen 0 and 1 which is the cut-off value for correlations
#        bins, an optional vector that contains one or more integers that represent the numbers of bins 
#              to use for a histogram. If the vector is not provided, use default.
# Output: as described in the previous helper functions.
explore <- function(dataframe, plot="off", threshold, bins=NULL){
  
  if (plot == 'on'){
    plotOn(dataframe,bins)
  }
  else if (plot == 'grid'){
    plotGrid(dataframe,bins)
  }
  
  result01 <- freqTable(dataframe)
  result02a <- sumTable(dataframe)
  result02b <- rSquares(dataframe)
  result02c <- pearCoefs(dataframe,threshold)
  
  results <- list(result01,result02a,result02b,result02c)
  return (results)
}

