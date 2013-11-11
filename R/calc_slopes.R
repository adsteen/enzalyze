##' Calculate slopes & fit statistics
##' 
##' @description Calculates slopes, intercepts, standard errors & other fit statistics for each unique combination of other variables.
##' @param time_col Column that contains the time, as elapsed time *Should have attr(time units!)

calc_slopes <- function(dm, time_col="Rtime", data_col="fl") {
  
  # Test that the units on the elapsed time column have been set
  if (is.null(attr(dm[, time_col}, "units"]))) {
    warning("Time units are unknown.")
  }
  
  
}