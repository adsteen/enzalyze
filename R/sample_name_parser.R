##' Parses sample names. Right now takes a data frame of 
##' 
##' @param names_vec A vector of names

sample_name_parser <- function(d, sample_name_col="sample.name", col_names=c("std.sample", "conc", "fluorophore", "buffer.sample")) {
  # d is meant to be a melted data frame of plate addresses and the name of the sample in the plate
  # d should have two columns: one with plate addresses (e.g. C6) and one with the text name of the sample (e.g. std, 50 uM, MUB)
  
  # Probably should move this out to read_plate_setup()
  names_vec <- d[ , sample_name_col]
  
  names_list <- strsplit(names_vec, split=", ") # replace with regular expression that will recognize multiple spaces
  parsed_df <- ldply(names_list, identity)
  
  
  sample_identifier <- cbind(d, parsed_df)
  
  #browser()
  sample_identifier
  
}