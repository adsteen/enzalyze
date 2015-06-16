#' Name substrates & calculate elapsed time
#' 
#' Relabel the substrates in the data frame, then calculate and insert elapsed times into a new column of the data frame
#' @param d - input data frame (in long form)
#' @param .labels - character string replacement for numeric labels of substrates
#' @param the.date - date.  Default results in the system date being called and used
#' @return returns the input data frame with two additional columns; one for the date-time object, and one for the calculated elapsed time (as a numeric vector)
#' @export

# this is a rough draft of the default variables, check with typical raw data structures
enzalyze_reform <- function(d, .labels = c("Arg-AMC", "Gly-AMC", "Leu-AMC", "Pyr-AMC", "GlyGlyArg-AMC"),
                            the.date = NULL){

  # Convert the subtrate numbers into names of substrate
  # This needs to not be hardcoded as the "substrate" column. potential for easy error such as 
  #   titling column as "Substrate"
  d$substrate <- factor(as.character(d$substrate), labels = .labels)
  
  # Convert fluorescence units to a numeric value so we can use them with the binary operations later
  d[ , "RFU"] <- as.numeric(gsub(",", "", d$RFU))
 
  # Process system date with custom date function
 the.date <- the_date(the.date = NULL)
  
  # paste concatenates our character vector "the.date" together with our numeric vector "time" from datm
  # ymd_hms converts our date-time vector to an object
  # then we store these objects into a new column in d
 # Note: need a try catch for whether the data format has seconds.  This parses data for hm
 #   format not hms
  d$Rtime <- ymd_hm(paste(the.date, d$time))
  
  # calculate elapsed time, for plotting purposes, as a numeric vector and store it in a new column of d
  d$elapsed <- as.numeric(d$Rtime - min(d$Rtime))
  
  # Attributes a unit to our numeric objects
  # d$elapsed <- attr(d$elapsed, "units") <- "seconds"
  attr(d$elapsed, "units") <- "seconds"
  
  # Return the data frame
  d
}