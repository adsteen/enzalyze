### Example of how to run data

d <- read_biotek()
plate_legend <- read_plate_setup()

# Parse the concentration vector in the legend
# plate_legend$conc <- parse_numeric(legend$conc)

# Merge the data and the legend
test <- merge(d, plate_legend, by="well")
