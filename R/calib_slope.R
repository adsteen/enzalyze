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
 
 
  # Using `lm_stats` for consistency and reliability, very easy to extract slope from regression stats
  cal <- lm_stats(d = d, xvar = xvar, yvar = yvar)
  
  # Extract the slope of the model
  cal_slopes <- cal["slope"]
  
  cal_slopes <- as.data.frame(cal_slopes)
  
  cal_slopes
 
}