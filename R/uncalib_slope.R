#' Uncalibrated Slope
#' 
#' Calculate the linear regression statistics, from the uncalibrated data, for each unique 
#' combination of id-variables, including the uncalibrated slopes (in units of RFU/hr)
#' @param d - data frame to be fed to plyr and lm_stats functions
#' @param id.var - variables to split input data frame by before applying the `lm_stats` function
#' @param time.var - independent variable for the linear model, representing the time label for the model (i.e "minutes", "seconds", "elapsed")
#' @param fluorescence - dependent variable for the linear model, representing the units of fluorescence from data collection
#' @return data frame containing the linear regression statistics for each combination of id-variables
#' @export

uncalib_slope <- function(d, id.var = c("rep", "treatment", "substrate"), 
                          time.var = "elapsed", fluorescence = "RFU"){
  
  # split a data frame by the specified id-variables and apply lm_stats, then return the result in a data frame
  lm_dframe <- ddply(d, .variables = id.var,
                     .fun = "lm_stats", xvar = time.var, yvar = fluorescence)
  
  lm_dframe
  
}