##' Reads in plate setup worksheets
##' 
##' @description Parses column names for tabular fluorescence data (where rows indicate separate sampling times)
##' @export
##' 
read_plate_setup <- function(d) {
  
  # New plan: 
  # 1. Get passed the raw data frame from .csv 
  # 2. Split the column names by columns
  # 3. Add a column to hte data frame for each variable from the column names. 
  #    Populate each column with data from the column names (using format "value units,")
  #nms <- col
  
  
}

# read_plate_setup <- function(fn=NA, ncol=13, 
#                              #key=c("std.or.sample", "conc", "fluorophore", "medium")) {
#                              key=NULL) {
#   
#   
#   
#   # Open dialog box if no file is specified
#   if (is.na(fn)) {
#     fn <- file.choose()
#   }
#   
#   # Read plate setup worksheet
#   d <- read.csv(fn)
#   first_col <- names(d)[1]
#   
#   # Melt df & create column of plate IDs
#   dm <- melt(d, id.vars=first_col, value.name="sample.name")
#   dm$well <- toupper(paste(dm$variable, dm[, first_col], sep=""))
#   #dm_skinny <- subset(dm, select="well", "value")
#   dm_skinny <- dm[ , c("well", "sample.name")]
#   
#   # Parse sample names
#    # I guess I need to write sample_name_parser as vectorized 
#   d_parsed <- sample_name_parser(dm_skinny)
#   
#   ###
#   # Assign names to columns. I'd like to do this automatically, but I'm not yet sure how
#   # For now, I'll assign them capital letters by default
#   ###
#   browser()
#   new.col.names <- c(key, LETTERS[1: (ncol(d_parsed)-length(key) - 2)])
#   names(d_parsed)[3:ncol(d_parsed)] <- new.col.names
#   
#   # Parse the concentration vector in the legend, if there is a conc column
#   if("conc" %in% names(d_parsed)) {
#     d_parsed$conc <- parse_numeric(d_parsed$conc)
#   }
#   
#   d_parsed
#   
# }
