##' Create plot of the Calibration Curve
##' 
##' @param data the data frame containing the calibration curve data
##' @param concentration.variable independent variable for the plot of the calibration curve
##' @param fluorescence variable dependent variable for the plot
##' @param curvelabel character string label for the plot title
##' @param site.code character string label specific to the sample or run, to be concatenated with the `curvelabel`
##' @return A plot of the calibration curve
##' @export

curve_plotr <- function(data, concentration.variable, fluorescence.variable,
                        curvelabel, site.code){
  
  ggplot(data, aes_string(x = concentration.variable, y = fluorescence.variable)) +
    geom_point() +
    geom_smooth(method="lm", se=TRUE) +
    ggtitle(paste0(curvelabel, site.code))
}