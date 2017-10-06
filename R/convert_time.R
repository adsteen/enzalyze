##' Converts time into machine format
##' 
##' @description Takes a data frame with time as strings. Adds a column called Rtime
##' @param dm Main data frame
##' @param date_col Column containing the date, as text <<or numeric?>>
##' @param time_col Column containing the time, as text <<or numeric?>>
##' @param format String indicating format of concatenated date and time
##' @export


#convert_time <- function(d, date_col=NA, time_col="time", format="ymd_hm") {
convert_time <- function(d, date_col=NA, time_col="time", format="ymd_hm") {
  
  # Drop case of "format" in case user uses caps for some reason
  format <- tolower(format)
  
  # Check that format is legal
  #   Not sure how to do this yet
  
  # Get the lubridate function to convert time - should be ymd_hms, or similar
  time_converter <- get(format) #Include error trapping please
  
  # Pull out date and time as separate columns
  if (is.na(date_col)) {
    date <- "1978-05-17"
  } else {
    date <- d[ , date_col]
  }
  time <- d[ , time_col]
  #browser()
  
  # Convert time appropriately 
  # INCLUDE ERROR TRAPPING
  # d$Rtime <- time_converter(paste(date, time), sep=" ")
  d$Rtime <- mdy_hm(paste(date, time))
  
  # Eliminate date and time columns
  cols_to_drop <- c(date_col, time_col)
  cols_to_keep <- ! (names(d) %in% cols_to_drop)
  
  d <- d[ , cols_to_keep] # I bet this will fail if date_col is NA
  
  d
  
}