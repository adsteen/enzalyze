##' Parses sample names. Right now takes a data frame of 
##' 
##' @param names_vec A vector of names

sample_name_parser <- function(d, sample_name_col="value", col_names=c("std.sample", "conc", "fluorophore", "buffer.sample")) {
  
  #####
  # Strategy 1
  #####
  # Probably should move this out to read_plate_setup()
  names_vec <- d[ , sample_name_col]
  
  names_list <- strsplit(names_vec, split=", ") # replace with regular expression that will recognize multiple spaces
  parsed_df <- ldply(names_list, identity)
  sample_identifier <- cbind(d, parsed_df)
  
  sample_identifier
  
}