#' Read raw data set as a data frame
#' 
#' Take in a raw data set, read as a data frame
#' @param x - raw dataset in textfile form
#' @return data frame
#' @export

read_long <- function(x){

  # create a filename with a path from the original filename "x" without NA values
  fn <- na.omit(x)
  d <- read.csv(fn, stringsAsFactors = FALSE)

}
