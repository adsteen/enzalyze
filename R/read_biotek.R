##' Reads .csv output from Sean's Biotek
##' Note that I don't really know what the output from that instrument really looks like
##' For now I'll use a csv file with headers as well addresses (A1, A2, etc)

read_biotek <- function(fn=NA, temp.var.name="T..365.450", time.var.name="Time..days.", 
                        temp.units="oC", time.units="days") {
  # Use dialog box if no file is specified
  if(is.na(fn)) {
    fn <- file.choose()
  }
  
  # Example csv "DS fictional biotek data.csv" has 3 reps of 16 concentrations of standard
  #   and multiple concentrations (different numbers) of +enzyme and control treatments of 2 different substrates (A-AMC and B-AMC)
  d <- read.csv(fn, na.strings=c("NA", "OVRFLW"), fileEncoding="latin1")
  
  d_tidy <- tidy_biotek(d)
  
  # The idea will be to read data from all instruments into a common format
  # Probably in the long term this should be an object
  # But for now it will be a data frame containing (for now) 4 columns:
  # Temp, Time (with an attr for units), well, and fl
  dm <- melt(d, id.vars=c(temp.var.name, time.var.name),
             variable.name="well", value.name="fl")
  
  # Check whether fl is numeric 
  if(!is.numeric(dm$fl)) {
    dm$fl <- as.numeric(dm$fl)
    warning("The fluorescence column is not read as numeric. Probably this is because an 'overflow' character was not set properly.")
  }
  
  # Rename temp and time columns, and set units (these will show up later in plots)
  names(dm)[names(dm)==temp.var.name] <- "temp"
  attr(dm$temp, "units") <- temp.units
  
  names(dm)[names(dm)==time.var.name] <- "time"
  attr(dm$time, "units") <- time.units
  
  
  dm
}