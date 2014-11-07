# rank the death causes by the total number of deaths
causeRank = function(data){
  # filter the Country column and the Total column to compute the 
  # summation of each death cause
  summation = colSums(data[, -c(1, 2)])
  
  rankList = sort(summation, decreasing = TRUE)
  # group and plot
  barplot(rankList)
  rankList
}

# rank the death causes by the variability of the number over countries
sdRank = function(data){
  # calculate the standard deviation of each death cause
  standardDeviation = apply(data[, -c(1, 2)], 2, sd)
  
  sdRankList = sort(standardDeviation, decreasing = TRUE)
  # we use a boxplot to better visualize the variability of each death cause
  boxplot(data[, -c(1, 2)])
  sdRankList
}

# we want to know what are the death causes that take the leading places of 
# death rate in the most country, say if NCD causes the most deaths in 88 
# countries, the value of it is definitely 88
commonCauseRankOfCountries = function(data){
  # for each row, we find out the index of the most leading death cause
  index = apply(data, 1, which.max)
  
  # group the leading death causes and calculate the counts
  summation = table(index)
  sortedSummation = sort(table(index), decreasing = TRUE)
  
  # the rest of the code block is to modify the names in each column
  tableIndex = names(sortedSummation)
  causesNames = names(data)
  count = 1
  for(index in tableIndex){
    index = as.numeric(index)
    names(sortedSummation)[count] = causesNames[index]
    count = count + 1
  }
  
  sortedSummation
}

# originally this function is implemented for the correlation analysis
normalization = function(df){
  featuresNum = ncol(df)
  normalizedData = df
  
  for(i in 2:featuresNum){
    m = mean(df[, i])
    s = sd(df[, i])
    
    normalizedData[, i] = (df[, i] - m) / s
  }
  normalizedData
}

# we expect the result correlation analysis is consistent with that of PCA
# but the result cannot make sense. we guess the reason is that the relationships
# between each death cause and factor are not linear, which caused distortions
# in the correlation analysis
correlationAnalysis = function(dataDir = "", datapath = ""){
  if(datapath != ""){
    currentData = read.csv(datapath)
    currentData = normalization(currentData)
    cor(currentData[, 2], currentData[, 3])
  }
  else if(dataDir != ""){
    dataFilenames = list.files(dataDir, pattern = "*.csv")
    
    correlationList = list()
    for(filename in dataFilenames){
      print(filename)
      currentData = read.csv(paste(dataDir, filename, sep = "/"))
      currentData = normalization(currentData)
      
      correlation = cov(currentData[, 2], currentData[, 3])
      key = names(currentData)[3]
      value = correlation
      
      correlationList[[key]] = value
    }
    correlationList
  }
  else{
    stop("Please enter the data filepath or the data directory.")
  }
}

# we do the PCA to find out the principal components that have strong relationships
# with the death rate
PCA = function(table)  
{
  # read the table
  x <- table
  # First we have to scale the table by: 
  #  x-scaled(ij) = (x - column mean)/column range 
  # the centering is automatically done, but we have to find the range.
  # First we have to find the number of columns, because that determines
  # the number of values that we have to put into the scale.vector:
  xncol <- ncol(x)
  # Now we find the scaling values, which are basically the column ranges:
  # First we create an empty vector:
  # range.vector=c()
  # Find the range values and place in a vector:
  min.col = vector()
  max.col = vector()
  
  # get the maximum and minimum values for feature scaling
  for (j in 1:xncol){
    min.col[j] <- min(x[,j])
    max.col[j] <- max(x[,j])    
  }
  rangediff <- max.col - min.col
  y <- scale(x, center = TRUE, scale=rangediff)
  # we do SVD to decompose the matrix since the matrix is not square matrix
  # we cannot use correlation or covariance to calculate eigenvalues and eigenvectors
  ysvd <- svd(y)
  
  # Calculate squared of the singular value...
  sqr.ysvd <- ysvd$d^2
  # ...and its %
  total.sqr.ysvd <- sum(sqr.ysvd)
  percent.ysvd <- (sqr.ysvd / total.sqr.ysvd) * 100
  
  # Plot the vector coordinates
  #plot <- plot(ysvd$v)
  
  ds = sum(y * y) # Data scatter
  mu1 <- round(ysvd$d[1] ^2) # Mu
  contr <- (mu1 / ds)*100 # Contribution of first component
  
  #answer <- list(ysvd$d, sqr.ysvd, percent.ysvd, ysvd$u, ysvd$v, ds, mu1, contr, plot)
  answer <- list(ysvd$d, percent.ysvd, ysvd$v, ds, mu1, contr)
  names(answer)[[1]] <- "Singular.value"
  names(answer)[[2]] <- "Singular.value.percent"
  names(answer)[[3]] <- "PC.vector.coordinates" # this is the sigular vector
  names(answer)[[4]] <- "Data.scatter"
  names(answer)[[5]] <- "Mu"
  names(answer)[[6]] <- "First.component.Contribution."
  
  return(answer)
}

# use inlined API to do the PCA analysis, but this result seems a little strange
PCAComplete = function(data){
  answer = prcomp(data, scale = TRUE)
  answer
}