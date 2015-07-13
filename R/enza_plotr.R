# ##' Create plots of raw data, the calibration curve, and the calibrated activities
# ##' 
# ##' @param plot.data
# ##' @param plot.curve
# ##' @param plot.v0
# ##' @param data
# ##' @param curve
# ##' @param activity
# ##' @param time.variable
# ##' @param fluorescence.variable
# ##' @param concentration.variable
# ##' @param site.code
# ##' @param datalabel
# ##' @param curvelabel
# ##' @param v0label
# ##' @return created plots
# ##' @export
# 
# enza_plotr <- function(plot.data = FALSE, plot.curve = FALSE, plot.v0 = FALSE,
#                        data, curve, activity, 
#                        time.variable, fluorescence.variable, concentration.variable,
#                        site.code, datalabel = "Raw Data site_", 
#                        curvelabel = "Calibration Curve For AMC, site ", 
#                        v0label = "Calibrated v0, site "
#                        ){
#  
#    if(plot.data) {
#      p_data <- ggplot(data, aes_string(x = time.variable, y = fluorescence.variable,
#                                        shape = "treatment", colour = "rep", fill = "rep")) +
#        geom_point() +
#        geom_smooth(method="lm", se=FALSE) +
#        facet_wrap( ~ substrate) + 
#        ggtitle(paste(datalabel, site.code))
#      
#     print(p_data)
#   }
#   
#   if(plot.curve) {
#     p_curve <- ggplot(curve, aes_string(x = concentration.variable, y = fluorescence.variable)) +
#       geom_point() +
#       geom_smooth(method="lm", se=TRUE) +
#       ggtitle(paste0(curvelabel, site.code))
#     
#     print(p_curve)
#   }
#   
#   if(plot.v0) {
#     p_activity <- ggplot(activity, aes_string(x = "substrate", y = "v0", colour = "rep",
#                                               shape = "treatment")) +
#       geom_pointrange(aes(ymin = v0 - v0.se, ymax = v0 + v0.se),
#                       position = position_jitter(width=0.2)) +
#       ylab(expression(paste(v[0], ", ", n, "M ", hr^{-1}))) + 
#       ggtitle(paste0(v0label, site.code))
#     
#     print(p_activity)
#   }
#   p_activity
# }