##' Reads in plate setup worksheets
##' 
##' @description For now: takes a filename, reads the file and melts it
##' @export

read_plate_setup <- function(fn="data/sample_plate_layout.csv", ncol=13, 
                             column.names=c("std.or.sample", "conc", "fluorophore", "medium")) {
  
  # Read plate setup worksheet
  d <- read.csv(fn)
  first_col <- names(d)[1]
  
  # Melt df & create column of plate IDs
  dm <- melt(d, id.vars=first_col, value.name="sample.name")
  dm$well <- toupper(paste(dm$variable, dm[, first_col], sep=""))
  #dm_skinny <- subset(dm, select="well", "value")
  dm_skinny <- dm[ , c("well", "sample.name")]
  
  # Parse sample names
   # I guess I need to write sample_name_parser as vectorized 
  d_parsed <- sample_name_parser(dm_skinny)
  
  ###
  # Assign names to columns. I'd like to do this automatically, but I'm not yet sure how
  ###
  
  new.col.names <- c(column.names, LETTERS[ncol(d_parsed)-length(column.names) - 2])
  names(d_parsed)[3:ncol(d_parsed)] <- new.col.names
  
  # browser()
  # Parse the concentration vector in the legend
  d_parsed$conc <- parse_numeric(d_parsed$conc)
  
  d_parsed
  
}
