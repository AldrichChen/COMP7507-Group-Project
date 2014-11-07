# the original format of the dataset is .xlsx that consists of 4 sheets
# it's difficult for importing data to visualization tools
# to avoid labor intensive works, we automatically transform xlsx files
# to csv files
fromXlxsToCSV = function(directory = "", filepath = ""){
  # import library to read xlsx files
  library("xlsx")
  
  # once the filepath is provided, we take this arguments to transform the single dataset
  if(filepath != ""){
    dataset = read.xlsx(filepath, sheetIndex = 1)
    # note that row.names should be FALSE, or the row id takes up the first column
    write.csv(dataset, sub("xlsx", "csv", filepath), row.names = FALSE)
  }
  # when filepath is empty
  else if(directory != ""){
    filenames = list.files(directory, pattern = "*.xlsx")
    
    # read all the xlsx files under the directory
    for(filename in filenames){
      dataset = read.xlsx(paste(directory, filename, sep = "/"), sheetIndex = 1)
      destpath = paste(directory, sub("xlsx", "csv", filename), sep = "/")
      write.csv(dataset, destpath, row.names = FALSE)
    }
  }
  else{
    stop("directory and filepath cannot be both empty.")
  }
}

# because the death cause dataset were collected in 2008, so we only
# want the factors dataset in 2008
# this function extracts out the 2008 column
getFactors2008 = function(directory = "", filepath = ""){
  library(tools)
  # we only need these two attributes, Country is the key
  keeps = c("Country", "X2008")
  
  if(filepath != ""){
    currentData = read.csv(filepath)
    # substring the factor name from the filepath
    fieldName = basename(file_path_sans_ext(filepath))
    
    # re-name the column names
    names(currentData)[1] = "Country"
    # subsetting currentData
    currentData = subset(currentData, select = keeps)
    names(currentData)[2] = fieldName
    
    # cover the original factor dataset
    write.csv(currentData, filepath, row.names = FALSE)
  }
  # extracts the data for 2008 in each factor dataset
  else if(directory != ""){
    filenames = list.files(directory)
    
    for(filepath in filenames){
      # get factor name
      fieldName = file_path_sans_ext(filepath)
      
      currentData = read.csv(paste(directory, filepath, sep = "/"))
      names(currentData)[1] = "Country"
      currentData = subset(currentData, select = keeps)
      names(currentData)[2] = fieldName
      
      write.csv(currentData, paste(directory, filepath, sep = "/"), row.names = FALSE)
    }
  }
  else{
    stop("directory and filepath cannot be both empty.")
  }
}

# the factor datasets come from different sources, some of the value are missing
# we need to filter the incomplete cases, so in this step we might lost some
# countries' data
filterNAs = function(directory = "", filepath = ""){
  if(filepath != ""){
    currentData = read.csv(filepath)
    # filter the rows contain NAs
    currentData = currentData[complete.cases(currentData), ]
    
    write.csv(currentData, filepath, row.names = FALSE)
  }
  else if(directory != ""){
    filenames = list.files(directory, pattern = "*.csv")
    
    for(filename in filenames){
      currentData = read.csv(paste(directory, filename, sep = "/"))
      # filter the rows contain NAs
      currentData = currentData[complete.cases(currentData), ]
      
      destpath = paste(directory, filename, sep = "/")
      write.csv(currentData, destpath, row.names = FALSE)
    }
  }
  else{
    stop("directory and filepath cannot be both empty.")
  }
}