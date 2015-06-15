#' Calibrated Slope
#' 
#' Calculate the slope of the calibration curve (in units of RFU per nM-AMC per L)
#' @param d - data frame
#' @param xvar - independent test variable
#' @param yvar - dependent test variable
#' @return one row data frame containing the slope of the calibration curve
#' @export

calib_slope <- function(d, xvar="conc.AMC.nM", yvar="RFU"){
  
  # safeguard the calculation of slopes
#   get_model <- function(d, xvar=xvar, yvar=yvar) {
#     model <- tryCatch(
#       model <- lm(d[ , xvar] ~ d[ , yvar]),
#       error=function(cond) return(NA),
#       warning=function(cond) return(model),
#       finally = {}
#     )
#     return(model)
#   }
 
 
  d$"RFU" <- as.numeric(gsub(",", "", d$"RFU"))
  model <- lm_stats(d = d, xvar = xvar, yvar = yvar)
  # Get the slope of the model
  cal_slopes <- model["slope"]
  cal_slopes
 

}