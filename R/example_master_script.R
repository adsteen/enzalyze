# ### Example of how to run data
# library(ggplot2)
# library(scales) #not sure if I use this
# library(reshape2)
# library(plyr)
# 
# source("R/read_biotek.R")
# source("R/read_plate_setup.R")
# source("R/sample_name_parser.R")
# source("R/parse_numeric.R")
# source("R/cal_curve.R")
# source("R/lm_stats.R")
# 
# 
# # preprocess_data <- function() {
# 
# label_key <- c("std.or.sample", "conc", "fluorophore", "medium") # User needs to include this
# 
# d <- read_biotek(fn="data/test_data_2.csv")
# 
# # I'm not sure this works correctly when no key is specified
# plate_legend <- read_plate_setup(fn="data/sample_plate_layout.csv", key=label_key)
# 
# # Merge the data and the legend - I should put this in a function; don't know where
# labeled_data <- merge(d, plate_legend, by="well")
# attr(labeled_data$conc, "units") <- attr(plate_legend$conc, "units")
# 
# # Calibrate the data 
# calibrated <- cal_curve(labeled_data, print.plot=TRUE, print.cal.stats=TRUE)
# variable_col_names <- label_key[-which(label_key=="std.or.sample")]
# 
# 
# 
# # Then calculate rates for each well (or maybe each combination of all relevant variables)
# # I think I should force the user to input a key for the 
# 
# 
# 
# #}
# 
# 
# 
