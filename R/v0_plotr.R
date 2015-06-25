##' Create Plot of v0 for various substrates
##' 
##' @export

v0_plotr <- function(data, v0label, site.code){
  
  ggplot(data, aes_string(x = "substrate", y = "v0", colour = "rep",
                              shape = "treatment")) +
    geom_pointrange(aes(ymin = v0 - v0.se, ymax = v0 + v0.se),
                    position = position_jitter(width=0.2)) +
    ylab(expression(paste(v[0], ", ", n, "M ", hr^{-1}))) + 
    ggtitle(paste0(v0label, site.code))
}