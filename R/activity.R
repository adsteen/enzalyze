##' Activity of enzymes
##' 
##' Calculates enzymatic activity from a raw, uncalibrated dataset and a calibration curve
##' @param uncal Raw dataset, of uncalibrated activity, in textfile form
##' @param cal Raw dataset containing the calibration curve
##' @param substrates Character string label to replace numeric label of substrates in data
##' @param .the.date The date the data was gathered (default will print the current date)
##' @param .id.var Vector of variables by which to split input data frame by, before applying lm_stats to calculate slope values
##' @param .xvar Independent variable for the uncalibrated linear model
##' @param .yvar Dependent variable for the linear model (same for uncalibrated & calibrated models)
##' @param xvar Independent variable for the calibration curve linear model
##' @return One-row data frame containing the activity of enzymes
##' @export

activity <- function(uncal, cal, substrates = c("Arg-AMC", "Gly-AMC", "Leu-AMC", "Pyr-AMC", "GlyGlyArg-AMC"), 
                     .the.date = NULL, .id.var = 
                       c("rep", "treatment", "substrate"), .xvar = "elapsed",
                     .yvar = "RFU", xvar="conc.AMC.nM"){
  
  # We want the input parameter "uncal" to match the parameter "x" of read_long
  d_uncal <- read_long(x = uncal)
  
  # Then the output data frame of read_long is "dat", so we set that as the new input
  #    parameter for enzalyze_reform; "d"
  dr_uncal <- enzalyze_reform(d = d_uncal, .labels = substrates, the.date = .the.date)
  
  # The output parameter from enzalyze_reform; "d", is redirected as the input data 
  #   frame for uncalib_slope
  lm_dframe <- uncalib_slope(d = dr_uncal, id.var = .id.var, xvar = .xvar,
                yvar = .yvar)
  
  ####
  #Now we need to produce a data frame to represent the slope of the calibration curve
  ####
  d_cal <- read_long(x = cal)
  
  # redirect the output dataframe to the function; calib_slope
  cal_slope <- calib_slope(d = d_cal, xvar = xvar, yvar = .yvar)
  
  lm_dframe$v0 <- lm_dframe$slope / cal_slope
  lm_dframe$v0.se <- lm_dframe$slope.se / cal_slope
  
  lm_dframe
  
}