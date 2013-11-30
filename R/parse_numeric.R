##' Parse numeric values and assign units
##' 
##' @param x A character *vector* of numeric values, with units.
##' @details The units must remain the same along the vector 
parse_numeric <- function(x) {
  
  # Include some warnings & error trapping if not string, etc.
  # use if(!is.na(as.numeric(x))) formulation
  
  num_list <- strsplit(x, split=" ") # could probably fancy this up with regular expressions
  
  # Pull out numbers and units into separate vectors
  num_vec <- laply(num_list, function(x) x[[1]])
  units_vec <- laply(num_list, function(x) x[[2]])
  
  # somehow test whether all the units along the vector are equal to each other
  # Not sure how to do this
  
  num_vec <- as.numeric(num_vec)
  attr(num_vec, units) <- units_vec[1]
  
  num_vec
  
}