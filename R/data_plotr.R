##' Create faceted plot of data
##' 
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