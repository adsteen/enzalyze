##' Calculates t profile using patented Steen method
##' 
##' @description Calculates t_opt profile based on single-pot heating experiment
##' @param d Data frame containing columns with raw fluorescence, time or elapsed time, and temp
##' @param yvar Column name (as string) for response variable (fl)
##' @param xvar Column name (as string) for time variable (as POSIXct or numeric)
##' @param tvar Column name (as string) for temperature variable
##' @details <<How this works>>
##' @example <<include some examples>>
##' @export
##' 

calc_t_profile <- function(d, yvar="fl", xvar="elapsed", tvar="temp") {
  
  # Pull out separate vectors in order to calculate rise and run
  n <- nrow(d)
  xi <- d[2:n, "elapsed"]
  xo <- d[1:(n-1), "elapsed"]
  yi <- d[2:n, "fl"]
  yo <- d[1:(n-1), "fl"]
  Ti <- d[2:n, "temp"]
  To <- d[1:n, "temp"]
  
  # Calculate slope as rise/run
  rise <- yi-yo
  run <- xi-xo # Should work with POSIXct _or_ numeric
  slope <- rise/run
  
  # Calculate average temp & average time
  temp_av <- To + (Ti-To)/2
  time_av <- xo + (xi+xo)/2
  
  t_profile <- data.frame(temp_av, time_av, slope=slope)
  t_profile
}
