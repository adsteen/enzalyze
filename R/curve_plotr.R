##' Create plot of the Calibration Curve
##' 
##' @export

curve_plotr <- function(data, concentration.variable, fluorescence.variable,
                        curvelabel, site.code){
  
  ggplot(data, aes_string(x = concentration.variable, y = fluorescence.variable)) +
    geom_point() +
    geom_smooth(method="lm", se=TRUE) +
    ggtitle(paste0(curvelabel, site.code))
}