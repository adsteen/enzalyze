### Example of how to run data

d <- read_biotek()
legend <- read_plate_setup()

# Need to move this into read_plate_setup() somehow
names(legend)[3:6] <- c("std.or.sample", "conc", "fluorophore", "medium")

# Merge the data and the legend
test <- merge(d, legend, by.x="variable", by.y="well")
