##' Activity of enzymes
##' 
##' Calculates enzymatic activity from a raw, uncalibrated dataset and a calibration curve
##' @param uncal Raw dataset, of uncalibrated activity, in textfile form
##' @param cal Raw dataset containing the calibration curve
##' @param .id Vector of non-measured variables to melt the data frame by
##' @param .var Name of variable used to store measured variable names
##' @param .val Variable used to store values
##' @param .the.date The date the data was gathered (default will print the current date)
##' @param .id.var Vector of variables by which to split input data frame by, before applying lm_stats to calculate slope values
##' @param .xvar Independent variable for the uncalibrated linear model
##' @param .yvar Dependent variable for the linear model (same for uncalibrated & calibrated models)
##' @param xvar Independent variable for the calibration curve linear model
##' @return One-row data frame containing the activity of enzymes
##' @export

activity <- function(uncal, cal, .id = c("rep", "treatment", "substrate"), .var = "well",
                     .val = "fluorescence", .the.date = NULL, .id.var = 
                       c("rep", "treatment", "substrate"), .xvar = "elapsed",
                     .yvar = "RFU", d, xvar="conc.AMC.nM"){
  
  # We want the input parameter "uncal" to match the parameter "x" of read_long
  d_uncal <- read_long(x = uncal)
  
  # Then the output data frame of read_long is "dat", so we set that as the new input
  #    parameter for enzalyze_reform; "d"
  enzalyze_reform(d = d_uncal, id = .id, var = .var, val = .val, the.date = .the.date)
  
  # The output parameter from enzalyze_reform; "datm", is redirected as the input data 
  #   frame for uncalib_slope
  uncalib_slope(d = datm, id.var = .id.var, xvar = .xvar,
                yvar = .yvar)
  # The output is a data frame called "uncal_slopes"
  
  ####
  #Now we need to produce a data frame to represent the slope of the calibration curve
  ####
  d_cal <- read_long(x = cal)
  
  # redirect the output dataframe to the function; calib_slope
  calib_slope(d = d_cal, xvar = xvar, yvar = .yvar)
  # the output data frame is under the name; "cal_slopes"
  
  # To find the true activity, divide the dataframe for uncalibrated slopes by the slope
  #   of the calibration curve
  a <- uncal_slopes / cal_slopes
  a
}