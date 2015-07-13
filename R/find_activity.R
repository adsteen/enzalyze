##' Find the calibrated activity (and activity standard-error) of enzymes
##' 
##' Calculates enzymatic activity from a raw, uncalibrated dataset and a calibration curve
##' @param uncal Raw dataset, of uncalibrated activity, in textfile form
##' @param cal Raw dataset containing the calibration curve
##' @param site.code desired label/destinction for data set
##' @param substrates Character string label to replace numeric label of substrates in data
##' @param .the.date The date the data was gathered (default will print the current date)
##' @param design.variables Test-specific design variables. Also serves as a vector of variables by which to split input data frame by, before applying lm_stats to calculate slope values
##' @param time.variable Independent variable for the uncalibrated linear model
##' @param fluorescence.variable Dependent variable for the linear model (same for uncalibrated & calibrated models)
##' @param concentration.variable Independent variable for the calibration curve linear model to represent the concentration
##' @return One-row data frame containing the activity of enzymes
##' @export

find_activity <- function(uncal, cal, site.code = NULL, substrates, .the.date = NULL,
                     design.variables = c("rep", "treatment", "substrate"),
                     time.variable = "elapsed", fluorescence.variable = "RFU",
                     concentration.variable = "conc.AMC.nM", print.plot = FALSE,
                     save.plot = FALSE, plot.filename = NULL, save.datafile = FALSE,
                     datafile.filename = NULL){
  
  #browser()
  # We want to read the data
  d_uncal <- read_long(x = uncal)
  
  # Need to replace numeric labels of substrates with names as character strings
  # Then `enzalyze_reform` calculates and inserts elapsed times for further plotting & calculation purposes;
  #   elapsed time is necessary for finding v0
  dr_uncal <- enzalyze_reform(d = d_uncal, .labels = substrates, the.date = .the.date)
  
  # Plot the raw data
  
  p_data <- data_plotr(dr_uncal, datalabel = "Raw Data site ", site.code = site.code, 
                       time.variable = time.variable, fluorescence.variable = fluorescence.variable,
                       shape = "treatment",
                       colour = "rep", fill = "rep")
  
  if(print.plot) {
    print(p_data)
  }

  if(save.plot) {
    ggsave(paste0("site_", site.code, "_raw_data.png"), p_data, height=4, width=6,
           units="in", dpi=300)
  }
  
  # Split data frame by design variables and apply `lm_stats` so we have easy access to regression statistics for every unique combination of design variables
  lm_dframe <- uncalib_slope(d = dr_uncal, id.var = design.variables, time.var = time.variable,
                fluorescence = fluorescence.variable)
  
  ####
  #Now we need to produce a data frame to represent the slope of the calibration curve
  ####
  d_cal <- read_long(x = cal)
  
  
  # Turn this into an if function with a default of TRUE
  d_cal[ , "RFU"] <- as.numeric(gsub(",", "", d_cal[ ,"RFU"]))
  
  # Create the option to print calibration curve so we can see if it looks good
  p_curve <- curve_plotr(data = d_cal, concentration.variable = concentration.variable, 
                         fluorescence.variable = fluorescence.variable, 
                         curvelabel = "Calibration Curve site ", site.code = site.code)
 
   if(print.plot) {
    print(p_curve)
  }
 
   if(save.plot) {
    ggsave(paste0("site_", site.code, "_calibration_curve.png"), p_curve, height=5,
           width=6, units="in", dpi=300)
  }
  
  # Now within this function we are applying `lm_stats` again solely because we know how to easily find any values we need
  cal_slope <- calib_slope(d = d_cal, xvar = concentration.variable, yvar = fluorescence.variable)
  
  
  # Here we throw in new columns for v0 & v0.se so both are printed with the regression statistics in the output
  lm_dframe$v0 <- lm_dframe$slope / cal_slope
  lm_dframe$v0.se <- lm_dframe$slope.se / cal_slope
  
  # Create a plots for v0
  p_activity <- v0_plotr(data = lm_dframe, v0label = "Calibrated v0 site ",
                         site.code = site.code)
  
  # If required, print and save them
  if(print.plot) {
    print(p_activity)
  }
  if(save.plot) {
    ggsave(paste0("site_", site.code, "_calibrated_v0.png"), p_activity, height=4, width=6,
           units="in", dpi=300)
  }
  
  lm_dframe
  
}