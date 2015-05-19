#' Read raw data set in long form
#' 
#' Take in a raw data set, read as a data frame, and melt into a long form data frame
#' @param x - raw dataset in textfile form
#' @return data frame
#' @export

read_long <- function(x){
  
  # create a filename with a path from the original filename by converting our object "x" and concatenating it with "data" as one character vector
  fn <- paste0("data/", x)
  
  # omit any NA values and read the textfile as a data frame
  fn <- na.omit(fn)
  dat <- read.csv(fn)
  
}