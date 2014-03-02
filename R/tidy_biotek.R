##' Puts biotek files into tidy form
##'
##' @description Formats raw biotek files into long form with standardized column headings
##' 

tidy_biotek <- function(d, temp.col="temp", time.col="Time..days.") {
  
  fn <- "data/DS_fictional_biotek_data_short.csv"
  
  # Note: Fictional experiment: 
  # 16 standards: 0:15 uM (columns 1:16), 3 reps each (48 total)
  # 2 substrates (A-AMC, B-AMC, C-AMC, etc.)
  # 3 live reps and 1 control rep for each substrate
  # 10 concentrations for each substrate
  # Crap: I forgot that read.csv bollixes the commas and spaces in column names
  
  # This should be passed in
  d <- read.table(fn, sep=",", header=TRUE) # Change to read.csv(fn)
  
  # Make a vector of the "legalized" column names
  d_colnames <- names(d)
  
  # Rename the dataset columns with V3:VX (1st 2 columns, time and temp, are ok)
  names(d)[3:ncol(d)] <- paste("V", 3:ncol(d), sep="")
  
  # Melt the data frame
  dm <- melt(d, id.vars=c(temp.col, time.col), variable.name="well", value.name="fl")
  
  # determine illegal column names
  names_to_parse <- data.frame(sample.name=t(read.table(fn, sep=",", header=FALSE, nrows=1)))
  names_to_parse$well <- rownames(names_to_parse)
    
  # Merge the datasets
  dataset <- merge(dm, names_to_parse, by="well")
  
  ####### 
  # Split data set into calibration and measured sets 
  #######
  
  # determine which readings are standards
  dataset$is.std <- FALSE # use regular expressions on colum sample.name
  dataset$is.std[grep("^std;", dataset$sample.name)] <- TRUE
  
  # split data frames into samples, stds
  standards <- dataset[dataset$is.std, ]
  samples <- dataset[!dataset$is.std, ]
  
  
  
  # So my strategy here is to read in the data frame without column names
  #    Default column names will be given: V1, V2, etc
  # Then read the column names a second time to get the column names as values of a data frame
  # Replace the id variables 
  # Merge those data frames
  # Eliminate the data frame of devault values
  
  
  

  
}