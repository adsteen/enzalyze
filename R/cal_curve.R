##' Pulls out calibration samples and calibrates them
##' 
##' @description Takes a labeled `raw' data frame 
##' (i.e., unprocessed but long-format and containing parsed labels for each well)
##' and returns  the same data frame, contiaining only samples (not standards), 
##' plus columns for calibrated fluorescence (in units of concentration of fluorophore) 
##' and associated error (plus p.value, etc). 
##' \textbf{Note:} I probably want to re-form these things as a list
cal_curve <- function(d, print.plot=TRUE, print.cal.stats=TRUE) {
  
  #browser()
  
  # In case there's no "fluorophore" column: add a fake one
  if(!"fluorophore" %in% names(d)) {
    warning("A column named 'fluorophore' was expected but not provided")
    d$fluorophore <- "unknown.fluorophore"
  }
 
  ######
  # Calculate standard curves for each fluorophore
  # Make plot
  ######
  cal_wells <- d[d$std.or.sample=="std" & !is.na(d$std.or.sample), ]
  
  #mean_calibs <- ddply(cal_wells, c(fluorophore.col, conc.col), summarise, mean_std=mean(fl, na.rm=TRUE))
  mean_calibs <- ddply(cal_wells, c("fluorophore", "conc"), summarise, 
                       mean_std=mean(fl, na.rm=TRUE),
                       sd_std=sd(fl, na.rm=TRUE))
  
  conc.units <- attr(d$conc, "units")
  
  p_cal_curve <- ggplot(mean_calibs, aes(x=conc, y=mean_std, colour=fluorophore)) + 
    geom_point() + 
    geom_smooth(method="lm", se=TRUE) +
    geom_errorbar(aes(ymin=mean_std-sd_std, ymax=mean_std+sd_std)) + 
    xlab(paste("conc,", conc.units)) + 
    ylab("RFU")
  
  if (print.plot) {
    print(p_cal_curve)
  }
  
  # Calculate slopes
  cal_curve_slopes <- ddply(mean_calibs, .(fluorophore), function(x) lm_stats(x, xvar="conc", yvar="mean_std"))
  
  # Output slopes
  if (print.cal.stats) {
    print(cal_curve_slopes)
  }
  
  ##########
  # Merge standard curve slopes back into sample data, return data only data frame
  ##########
  
  samples <- d[d$std.or.sample!="std" & !is.na(d$std.or.sample), ]
  calibrated_samples <- merge(samples, cal_curve_slopes, by="fluorophore") 
  
  # Calibrate the fluorescence values
  calibrated_samples$cal_fl <- (calibrated_samples$fl - calibrated_samples$int) / calibrated_samples$slope
  attr(calibrated_samples$cal_fl, "units") <- attr(d$conc, "units")
  
  calibrated_samples
  
}