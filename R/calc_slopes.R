##' Calculate slopes & fit statistics
##' 
##' @description Calculates slopes, intercepts, standard errors & other fit statistics for each unique combination of other variables.
##' @param dm Data frame IN LONG FORMAT containing time & fluorescence data, as well as metadata (substrate, treatment, location, etc.)
##' @param time_col Column that contains the time, as POSIXct
##' @param response_col Column that contains the 
##' @param time_units Units with which to express elapsed time (output)
##' @param cols_to_ignore Columns to ignore
##' @export

calc_slopes <- function(dm, time_col="Rtime", response_col="fl", time_units="hours", cols_to_ignore=c("std", "air.temp")) {
  
#   # Test that the units on the elapsed time column have been set
#   if (is.null(attr(dm[, time_col], "units"]))) {
#     warning("Time units are unknown.")
#   }
  
  # Test that time_col is POSIXct, and response_col is numeric
  if(!class(dm[ , time_col])[1]=="POSIXct") {
    stop(paste("Column ", time_col, " is not in the POSIXct (computer time) format.", sep=""))
  }
  
  if(!is.numeric(dm[ , response_col])) {
    stop(paste("column", response_col, " is not in numeric format.", sep=""))
  }
  
  data_cols <- c(time_col, response_col)
  
  metadata_cols <- names(dm)[!(names(dm) %in% c(data_cols, cols_to_ignore))]
  
  # Calc elapsed time
  dm <- ddply(dm, metadata_cols, transform, elapsed=as.numeric(Rtime - min(Rtime))) #outputs elapsed time in seconds
  
  # Divide elapsed time & set units appropriately, depending on desired time units
  div_factor <- switch(time_units,
                       seconds=1,
                       minutes=60,
                       hours=3600,
                       days=(3600*24))
  if (is.null(div_factor)) {
    warning(paste("Time units of ", time_units, " are not recognized. Setting units to seconds."))
    div_factor <- 1
    time_units <- "seconds (input misunderstood)"
  }
  dm$elapsed <- dm$elapsed/div_factor
  attr(dm$elapsed, "units") <- time_units
  
  # Calc slopes
  slopes <- ddply(dm, metadata_cols, function(x) lm_stats(x, xvar="elapsed", yvar=response_col))

}