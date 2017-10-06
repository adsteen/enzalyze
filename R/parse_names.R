##' Parses names of samples (or stds)
##'
##' @description Parses names of samples
##' 
##' @details Explain rules here (; separation, 2 elements)
##' 
parse_names <- function(d, name.col="sample.name", separator=";") {
  
  sample_names <- d[ , name.col]
  
  # Would be nice to include code as to which sample/standard set is bad
  cols_to_add <- tryCatch({
    ldply(strsplit(sample_names, split=separator)) # wrap in trycatch!
    }, warning=function(w) {
      print(paste("Warning function, ", w))
    }, error=function(e) {
      print(paste("Error function, ", e))
    }, finally={
      #print("helloooo!")
    })
  
  # NOT YET IMPLEMENTED
  # Parse names to change column header into useful name, & strip out units
  
  # Return original data frame with added columns for each variable
  cbind(d, cols_to_add)
  
}