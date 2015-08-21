##' Create faceted plot of data
##' 
##' A tool for plotting raw fluorescence readings vs elapsed time for each substrate tested
##' @param data the data frame used to create the plot
##' @param datalabel a character string title for the plot
##' @param site.code a character string label specific to the sample or run, to be joined with the plot title
##' @param time.variable the independent variable in creating the plot
##' @param fluorescence.variable the dependent variable in the plots
##' @param shape ggplot parameter
##' @param colour ggplot parameter
##' @param fill ggplot parameter
##' @return a faceted plot of the time variable vs the fluorescence variable, for each substrate
##' @export



data_plotr <- function(data, datalabel = "Raw Data site", site.code, 
                       time.variable, fluorescence.variable, shape = "treatment",
                       colour = "rep", fill = "rep"){
  ggplot(data, aes_string(x = time.variable, y = fluorescence.variable,
                          shape = shape, colour = colour, fill = fill)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    facet_wrap( ~ substrate) + 
    ggtitle(paste(datalabel, site.code))
}