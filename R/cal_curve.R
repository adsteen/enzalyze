##' Pulls out calibration samples and calibrates them
##' 
cal_curve <- function(d, std.col="std.or.sample", std.val="std", conc.col="conc", fluorophore.col="fluorophore") {
  # Pull out the calibration wells
  cal_wells <- d[d[ , std.col] == std.val, ]
  
  ddply()
  
}