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

activity <- function(uncal, cal, site.code = NULL, substrates, .the.date = NULL,
                     design.variables = c("rep", "treatment", "substrate"),
                     time.variable = "elapsed", fluorescence.variable = "RFU",
                     concentration.variable = "conc.AMC.nM", print.plot = FALSE,
                     save.plot = FALSE, plot.filename = NULL, save.datafile = FALSE,
                     datafile.filename = NULL){
  
  
#   browser()
  # We want the input parameter "uncal" to match the parameter "x" of read_long
  d_uncal <- read_long(x = uncal)
  
  # Then the output data frame of read_long is "dat", so we set that as the new input
  #    parameter for enzalyze_reform; "d"
  dr_uncal <- enzalyze_reform(d = d_uncal, .labels = substrates, the.date = .the.date)
  
  # Plot the raw data
  p_data <- ggplot(dr_uncal, aes_string(x = time.variable, y = fluorescence.variable,
                                shape = "treatment", colour = "rep", fill = "rep")) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    facet_wrap( ~ substrate) + 
    ggtitle(paste("Raw Data site_", site.code))
  if(print.plot) {
    print(p_data)
  }
  if(save.plot) {
    ggsave(paste0("site_", site.code, "_raw_data.png"), p_data, height=4, width=6,
           units="in", dpi=300)
  }
  
  # The output parameter from enzalyze_reform; "d", is redirected as the input data 
  #   frame for uncalib_slope
  lm_dframe <- uncalib_slope(d = dr_uncal, id.var = design.variables, time.var = time.variable,
                fluorescence = fluorescence.variable)
  
  # Plot the uncalibrated data
  p_uncal <- ggplot(lm_dframe, aes_string(x = "substrate", y = "slope", colour = "rep",
                                    shape = "treatment")) +
    geom_pointrange(aes(ymin = slope - slope.se, ymax = slope + slope.se),
                    position = position_jitter(width=0.2)) +
    ylab(expression(paste(v[0], ", ", n, "M ", hr^{-1}))) +
    ggtitle(paste("Uncalibrated v0 site_", site.code))
  if(print.plot) {
    print(p_uncal)
  }
  if(save.plot) {
   ggsave(paste0("site_", site.code, "_uncal_stats.png"), p_uncal, height=4, width=6,
          units="in", dpi=300)
  }
  
  ####
  #Now we need to produce a data frame to represent the slope of the calibration curve
  ####
  d_cal <- read_long(x = cal)
  
  d_cal[ , "RFU"] <- as.numeric(gsub(",", "", d_cal[ ,"RFU"]))
  
  # Look at the calibration data. Does it look good?
  p_calib <- ggplot(d_cal, aes_string(x = concentration.variable, y = fluorescence.variable)) +
    geom_point() +
    geom_smooth(method="lm", se=TRUE) +
    ggtitle(paste0("Calibration Curve For AMC, site ", site.code))
  if(print.plot) {
    print(p_calib)
  }
  if(save.plot) {
    ggsave(paste0("site_", site.code, "_calibration_curve.png"), p_calib, height=5,
           width=6, units="in", dpi=300)
  }
  
  # redirect the output dataframe to the function; calib_slope
  cal_slope <- calib_slope(d = d_cal, xvar = concentration.variable, yvar = fluorescence.variable)
  
  
  
  lm_dframe$v0 <- lm_dframe$slope / cal_slope
  lm_dframe$v0.se <- lm_dframe$slope.se / cal_slope
  
  # print plots for activity
  p_calibrated <- ggplot(lm_dframe, aes_string(x = "substrate", y = "v0", colour = "rep",
                                        shape = "treatment")) +
    geom_pointrange(aes(ymin = v0 - v0.se, ymax = v0 + v0.se),
                    position = position_jitter(width=0.2)) +
    ylab(expression(paste(v[0], ", ", n, "M ", hr^{-1}))) + 
    ggtitle(paste0("Calibrated v0, site ", site.code))
  if(print.plot) {
    print(p_calibrated)
  }
  if(save.plot) {
    ggsave(paste0("calibrated_v0, site ", site.code), p_calibrated, height=4, width=6,
           units="in", dpi=300)
  }
  
  lm_dframe
  
}