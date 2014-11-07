mergeAllCauses = function(directory, destpath = "./Merged.csv"){
  # get all the csv files under the directory
  filenames = list.files(directory, pattern = "Child-*.csv")
  ##print(filenames)
  
  # flag the first death cause dataset
  isFirstFlag = TRUE
  for(filepath in filenames){
    currentDataset = read.csv(paste(directory, filepath, sep = "/"))
    ##colNames = names(currentDataset)
    
    ##print(names(currentDataset))
    # this block extracts the attribute "Country" as the key
    # so here we do not need to merge
    if(isFirstFlag){
      destDataset = currentDataset
      isFirstFlag = FALSE
      
      # jump to the next loop
      next
    }
    
    # merge all the following death causes to the destDataset
    destDataset = merge(destDataset, currentDataset, by = "Country")
  }
  
  # in the original total death cause, there's deviation between 
  # the total number of deaths and the sum of other 9 single causes
  # for the sake of precision, we replace the Total field by row sums
  destDataset$Total = rowSums(destDataset[, 3:12])
  
  #if(destpath == ""){
    #write.csv(destData, destpath, row.names = FALSE)
  #}
  #else{
    #write.csv(destData, "", row.names = FALSE)
  #}
  write.csv(destData, destpath, row.names = FALSE)
  destDataset
}

# merge a single pair of <death cause, factor> dataset
mergeFactorAndCause = function(factorpath = "", causepath = "", destpath = ""){
  if(factorpath != "" && causepath != ""){
    factorData = read.csv(factorpath)
    causeData = read.csv(causepath)
    
    # merge the death cause and factor by "Country"
    destData = merge(causeData, factorData, by = "Country")
    
    # if destDatapath is not provided, simply return the destData
    # or write to the destpath
    if(destpath == ""){
      destData
    }
    else{
      write.csv(destData, destpath, row.names = FALSE)
    }
  }
  else{
    stop("Please enter the factor filepath and the cause filepath.")
  }
}

# merge all factors to on single cause, each pair is a single dataset
# say we have 8 factors, and we will get 1 * 8 <death cause, facotr> pair dataset
mergeFactorsAndSingleCause = function(factorDir = "", causepath = "", destDir = ""){
  if(factorDir != "" && causepath != "" && destDir != ""){
    # library for filepath parsing
    library(tools)
    
    factorFilenames = list.files(factorDir)
    causeData = read.csv(causepath)
    # get the cause label, file_path_sans_ext() for filtering the postfix
    # basename for getting the pure filename
    causeName = basename(file_path_sans_ext(causepath))
    
    isFirstMerge = TRUE
    for(factorpath in factorFilenames){
      factorData = read.csv(paste(factorDir, factorpath, sep = "/"))
      factorName = file_path_sans_ext(factorpath)
      
      # this block extracts the attribute "Country" as the key
      # so here we do not need to merge
      if(isFirstMerge){
        destData = merge(causeData, factorData, by = "Country")
        isFirstMerge = FALSE
      }
      else{
        destData = merge(destData, factorData, by = "Country")
      }
      
    }
    
    if(destDir == ""){
      # construct the destfile path
      destFilename = paste(paste(causeName, "All_Factors", sep = "_"), "csv", sep = ".")
      write.csv(destData, paste(destDir, destFilename, sep = "/"), row.names = FALSE)
    }
    else{
      stop("parameter destDir cannot be empty")
    }
  }
}

# merge each single factor to single death cause respectively
# say we have 11 causes dataset including the total death cause data
# and 8 factors, we will have 8*11 <death cause, factor> csv dataset
mergeAllFactors2AllCauses = function(factorDir = "", causeDir = "", destDir = ""){
  if(factorDir != "" && causeDir != "" && destDir != ""){
    library(tools)
    
    # get names of all death cause datasets
    causeFileNames = list.files(causeDir)
    for(filepath in causeFileNames){
      # concatenate the death cause filepath
      causepath = paste(causeDir, filepath, sep = "/")
      mergeFactorsAndSingleCause(factorDir = factorDir, causepath = causepath, destDir = destDir)
    }
  }
}

# this function is abandoned
mergeBatchFactorsAndCauses = function(factorDir = "", causeDir = "", destDir = ""){
  if(factorDir != "" && causeDir != "" && destDir != ""){
    factorFilenames = list.files(factorDir)
    causeFilenames = list.files(causeDir)
    library(tools)
    
    for(factorpath in factorFilenames){
      factorData = read.csv(paste(factorDir, factorpath, sep = "/"))
      factorName = file_path_sans_ext(factorpath)
      
      for(causepath in causeFilenames){
        causeData = read.csv(paste(causeDir, causepath, sep = "/"))
        causeName = file_path_sans_ext(causepath)
        
        destData = merge(causeData, factorData, by = "Country")
      
        destFilename = paste(paste(causeName, "and", factorName, sep = "_"), 
                             "csv", sep = ".")
        write.csv(destData, paste(destDir, destFilename, sep = "/"), row.names = FALSE)
      }
    }
  }
}

# just for test
writeToFile = function(destpath, data){
  write.csv(data, destpath, row.names = FALSE)
}