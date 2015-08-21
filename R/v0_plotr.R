##' Create Plot of v0 for various substrates
##' 
##' @param data data frame containing the linear regression statistics; including v0 and v0.se
##' @param v0label character string label for the v0 plot
##' @param site.code character string label specific to the sample or run, to be concatenated with `v0label` in the title
##' @export

v0_plotr <- function(data, v0label, site.code){
  
  ggplot(data, aes_string(x = "substrate", y = "v0", colour = "rep",
                              shape = "treatment")) +
    geom_pointrange(aes(ymin = v0 - v0.se, ymax = v0 + v0.se),
                    position = position_jitter(width=0.2)) +
    ylab(expression(paste(v[0], ", ", n, "M ", hr^{-1}))) + 
    ggtitle(paste0(v0label, site.code))
}