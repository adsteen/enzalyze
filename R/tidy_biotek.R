##' Puts biotek files into tidy form
##'
##' @description Formats raw biotek files into long form with standardized column headings
##' @param x - file name of data set (.csv)
##' @param n - number of rows to read as data frame from text file
##' @param d - horizontal data frame
##' @return ***put something here***

tidy_biotek <- function(x, n, d, temp.col="temp", time.col="Time..days.") {
  
  fn <- paste0("data/", x)
  
  # Note: Fictional experiment: 
  # 16 standards: 0:15 uM (columns 1:16), 3 reps each (48 total)
  # 2 substrates (A-AMC, B-AMC, C-AMC, etc.)
  # 3 live reps and 1 control rep for each substrate
  # 10 concentrations for each substrate
  # Crap: I forgot that read.csv bollixes the commas and spaces in column names
  
  # This should be passed in
  d <- read.csv(fn, nrows=n)
  
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
  dataset$sample.name <- as.character(dataset$sample.name)
  
  ####### 
  # Split data set into calibration and measured sets 
  #######
  
  # determine which readings are standards
  dataset$is.std <- FALSE # use regular expressions on colum sample.name
  dataset$is.std[grep("^std;", dataset$sample.name)] <- TRUE
  
  # split data frames into samples, stds
  standards <- dataset[dataset$is.std, ]
  samples <- dataset[!dataset$is.std, ]
  all_data <- list(standards=standards, samples=samples)
  
  #dataset$is.std <- as.factor(dataset$is.std)
  #split_data <- dlply(dataset, "is.std", identity)# The hell is the problem here?
  
  parsed_data <- llply(all_data, parse_names)

}