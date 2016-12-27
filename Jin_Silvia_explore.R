#########################
## Silvia Jin
## MATH 510 HW 07
## Jin_Silvia_explore.R
#########################

##Prof G - Nice work. The only small issue
##Prof G - is that the factor bar graphs did
##Prof G - not plot on a grid.

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
  tb1 <- lapply(dataframe[,sapply(dataframe,is.logical)],table) #draw the table for logical variables
  tb2 <- lapply(dataframe[,sapply(dataframe,is.factor)],table) #draw the table for factor variables
  return(list(tb1,tb2))
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
  num <- Filter(is.numeric,dataframe) #filter all numeric variables
  return(summary(num)) #return their summary table
}

#########################################
### Part 2b.
# function: rSquares
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
  pairNames <- c() # create a null binVector to store the names later
  pairRSquares <- c() # create a null binVector to store the R squares later
  l <- ncol(numericData) # store the number of cols, to be used in the for loop
  for (i in 1:(l-1)){ # looking at each numeric col
    for (j in (i+1):l){ # compare each numeric col with each numeric col after it (no repetation)
      fit <- lm(numericData[[i]] ~ numericData[[j]])
      pairNames <- c(pairNames,paste(numericNames[i],numericNames[j],sep='-')) # create the name of the pair and store it
      pairRSquares <- c(pairRSquares,summary(fit)$r.square) 
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
pearCoefs <- function(dataframe,threshold = NULL){
  numericData <- dataframe[sapply(dataframe,is.numeric)] # select the numeric cols and store into a new frame
  numericNames <- colnames(numericData)# extract the numeric col names
  pairNames <- c() # create a null binVector to store the names later
  pairCoeffs <- c() # create a null binVector to store the coeffs later
  l <- ncol(numericData) # store the number of cols, to be used in the for loop
  for (i in 1:(l-1)){ # looking at each numeric col
    for (j in (i+1):l){# compare each numeric col with each numeric col after it (no repetation)
      corcoef <- cor(numericData[i],numericData[j],method="pearson")
      if (is.null(threshold)){
        pairNames <- c(pairNames,paste(numericNames[i],numericNames[j],sep='-')) # create the name of the pair and store it
        pairCoeffs <- c(pairCoeffs,corcoef) # store the correlation coefficient
      }
      else if (abs(corcoef)>threshold){ # check if the absolute value of the calculated correlation is greater than the threshold
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#######

plotNumericOn <- function(dataframe,binVector=NULL){
  numericData <- dataframe[sapply(dataframe,is.numeric)]
  if(!is.null(binVector)){ # if binVector is not NULL
    for(j in 1:length(binVector)){ 
      for(i in 1:ncol(numericData)){
        mean <- mean(numericData[,i]) 
        # caculate the mean of each numeric column
        p1 <- ggplot(numericData,aes(x=numericData[i]),color = "blue")+ 
          #draw the histogram of count
          geom_histogram(fill="blue",bins=binVector[j])+
          ggtitle(paste(colnames(numericData[i]),binVector[j],sep=" bins="))+
          xlab(colnames(numericData[i]))+
          geom_vline(xintercept = mean,col="red")  
        #geom_vline can add red line on it
        
        p2 <- ggplot(numericData,aes(x=numericData[i],..density..))+
          #draw the density histogram
          geom_histogram(fill="blue",bins=binVector[j])+
          ggtitle(paste(colnames(numericData[i]),binVector[j],sep=" bins="))+
          xlab(colnames(numericData[i]))+
          geom_vline(xintercept = mean,col="red") 
        
        grid.newpage()
        #new page
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(numericData[i]),binVector[j],sep=" bin=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
        
      }
    }
  }else{ #if binVector is NULL
    for(i in 1:ncol(numericData)){
      mean <- mean(numericData[,i]) 
      #caculate the mean of each numeric column
      p1 <- ggplot(numericData,aes(x=numericData[i]),color = "blue")+  
        geom_histogram(fill="blue")+
        #draw the histogram of count
        ggtitle(paste(colnames(numericData[i]),"default bins",sep=" bins="))+
        xlab(colnames(numericData[i]))+
        geom_vline(xintercept = mean,col="red")
      p2 <- ggplot(numericData,aes(x=numericData[i],..density..))+
        #draw the density histogram
        geom_histogram(fill="blue")+
        ggtitle(paste(colnames(numericData[i]),"default bins",sep=" bins="))+
        xlab(colnames(numericData[i]))+
        geom_vline(xintercept = mean,col="red")
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
      title <- paste(colnames(numericData[i]),"default bins",sep=" bins=")
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
      print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
    }
  }
}

#

plotNumericGrid <- function(dataframe, binVector=NULL){
  numericData <- dataframe[sapply(dataframe,is.numeric)] 
  if (is.null(binVector)){ # if binVector is null
    grid.newpage()
    his_count <-list()   
    his_density <- list() 
    for(i in 1:ncol(numericData)){
      his_count[[i]] <- ggplot(numericData, aes_string(colnames(numericData[i])), color = "blue") + 
        geom_histogram(fill="blue")+ 
        ylab(NULL) + 
        labs(title= 'Count, default bins') 
      #draw histograms of count and add them to list his_count
    }
    multiplot(plotlist = his_count, cols = 2)  
    #draw all histogram with same bins in one page
    for(i in 1:ncol(numericData)){
      his_density[[i]] <- ggplot(numericData, aes_string(colnames(numericData[i])), color = "blue") + 
        geom_histogram(aes(y= ..density..), fill="blue")+ 
        ylab(NULL) + 
        labs(title= 'Density, default bins') 
      #draw histograms of density and add them to list his_density 
    }
    multiplot(plotlist = his_density, cols = 2)  
  }
  else { # if binVector is not null
    for(j in 1:length(binVector)){
      grid.newpage()
      his_count <-list()   
      his_density <- list()  
      #create two empty list
      for(i in 1:ncol(numericData)){
        his_count[[i]] <- ggplot(numericData, aes_string(colnames(numericData[i])), color = "blue") + 
          geom_histogram(fill="blue", bins = binVector[j])+ 
          labs(title= paste(binVector[j], "bins")) 
        #draw histograms of count and add them to list his_count
      }
      multiplot(plotlist = his_count, cols = 2)  
      #draw all histogram with same bins in one page
      for(i in 1:ncol(numericData)){
        his_density[[i]] <- ggplot(numericData, aes_string(colnames(numericData[i])), color = "blue") + 
          geom_histogram(aes(y= ..density..), fill="blue", bins = binVector[j])+ 
          labs(title= paste(binVector[j], "bins")) 
        #draw histograms of density and add them to list his_density 
      }
  }
    multiplot(plotlist = his_density, cols = 2)  
    #similar to above, draw all histogram of density with same bins in one page
  }
}

is.binary <- function(vector) {
  #This function will be used in the function plot_gray.
  #This function can tell whether the vector is a binary vector
  
  #Parameter: a vector
  
  #Returns: TRUE if the vector is binary, FALSE else
  x <- unique(vector)                    
  #x contains all unique values in v
  length(x) - sum(is.na(x)) == 2L         
  #check to see if x only contains 2 distinct values
}


plotCat <- function(dataframe) {
  facData <- dataframe[,sapply(dataframe,is.factor)]
  logData <- dataframe[,sapply(dataframe,is.logical)] 
  binData <- dataframe[,sapply(dataframe,is.binary)]
  flbData <- data.frame(facData,logData,binData)
  for(i in 1:ncol(flbData)){
    p <- ggplot(dataframe,aes(x=flbData[,i]))+
      geom_bar(fill='gray')+
      xlab(colnames(flbData)[i])
    print(p)
  }
}
# function: plotOn
# This is a helper function dealing with part 3 and 4 in the project.
# This is the function called directly in explore().
# Input: dataframe, the data that we are using
#        bins, a binVector of the bin numbers
# Plots a pair of blue histograms with a vertical red line at the mean (one using counts and the other density)
# for every numerical variable at each number of bins integer specified in the bin binVector parameter.
# Plots a gray bar graph for every categorical and binary variable.
plotOn <- function(dataframe,bins=NULL){
  plotNumericOn(dataframe, binVector = bins)
  plotCat(dataframe)
}

# function: plotGrid
# This is a helper function dealing with part 3 and 4 in the project.
# This is the function called directly in explore().
# Input: dataframe, the data that we are using
#        bins, a binVector of the bin numbers
# Plots a blue histogram with a vertical red line at the mean (one using counts and the other density)
# for every numerical variable at each number of bins integer specified in the bin binVector parameter. There
# is a grid for each count-bin combination and a separate grid for each density-bin size combination.
# Plots a gray bar graph for every categorical and binary variable.
plotGrid <- function(dataframe,bins=NULL){
  plotNumericGrid(dataframe, binVector = bins)
  plotCat(dataframe)
}


#########################################
### main function #######################
#########################################

# function: explore
# Input: dataframe, the data that we are using
#        plotswitch, off, on, or grid
#        threshold, a number bettwen 0 and 1 which is the cut-off value for correlations
#        bins, an optional binVector that contains one or more integers that represent the numbers of bins 
#              to use for a histogram. If the binVector is not provided, use default.
# Output: as described in the previous helper functions.
explore <- function(dataframe, plot="off", threshold=NULL, bins=NULL){
  
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

#########################################
### HW 08
### Exceptions and defensive codes
#########################################


