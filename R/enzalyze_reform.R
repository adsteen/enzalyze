#' Melt and reshape
#' 
#' Melt the data frame, calculate, and insert elapsed times into a long-form data frame
#' @param d - input data frame (in long form)
#' @param id - vector of non-measured variables to be fed into the melt function
#' @param var - name of variable used to store measured variable names
#' @param val - variable used to store values
#' @return returns the input data frame with two additional columns; one for the date-time object, and one for the calculated elapsed time (as a numeric vector)
#' @export

# this is a rough draft of the default variables, check with typical raw data structures
enzalyze_reform <- function(d, id = c("rep", "treatment", "substrate"), var = "substrate", 
                            val = "fluorescence", the.date=NULL){

  browser()
  
  # melt the data frame into long form
  datm <- melt(d, id.vars= id, variable.name= var, value.name=val)
  
  # Process system date
  if(is.null(the.date)) {
    today <- Sys.Date()
    the.date <- as.character(today)
  }
  
  # paste concatenates our character vector "the.date" together with our numeric vector "time" from datm
  # ymd_hms converts our date-time vector to an object
  # then we store these objects into a new column in datm
  datm$Rtime <- ymd_hms(paste(the.date, datm$time))
  
  # calculate elapsed time as a numeric vector and store it in a new column of datm
  datm$elapsed <- as.numeric(d$Rtime - min(d$Rtime))
  
  # Attributes a unit to our numeric objects
  attr(datm$elapsed, "units") <- "seconds"
}