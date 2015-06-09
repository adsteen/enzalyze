#' Uncalibrated Slope
#' 
#' Calculate the uncalibrated slope of each combination of id-variables (in units of RFU/hr)
#' @param d - data frame to be fed to plyr and lm_stats functions
#' @param id.var - variables to split input data frame by before applying lm_stats for the slope
#' @param xvar - independent variable for the linear model
#' @param yvar - dependent variable for the linear model
#' @return data frame containing the slope of each unique combination of id-variables
#' @export

uncalib_slope <- function(d, id.var = c("rep", "treatment", "substrate"), xvar = "elapsed", yvar = "RFU"){
  
  # split a data frame by the specified id-variables and apply lm_stats, then return the result in a data frame
  lm_dframe <- ddply(.data = d, .variables = id.var, function(x) lm_stats(x, xvar, yvar))
  
  # extract the slopes 
  # import data to test which way works, i think the third
  #uncal_slopes <- coef(lm_dframe)[2]
  #uncal_slopes <- lm_stats["slope"]
  uncal_slopes <- lm_dframe["slope"]
}