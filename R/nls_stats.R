##' Returns parameters of an nls model, or an empty data frame if it can't
##' Note this only works if it is a Michaelis-Menten nls model with parameters Vmax and Km
##' 
##' # SI should change this to return a vector so that you can use it directly with summarise

nls_stats <- function(x) {
  # Takes an object. Returns data frame of Vmax and Km if it is nls, NAs otherwise
  # Note it will fail if the object is NLS but not of mm model
  if(class(x)=="nls") {
    nls_coefs <- summary(x)$coefficients
    nls_params <- data.frame(Vmax=nls_coefs["Vmax", 1],
                             Vmax.std.err = nls_coefs["Vmax", 2],
                             Vmax.p=nls_coefs["Vmax", 4],
                             Km=nls_coefs["Km", 1], 
                             Km.std.err=nls_coefs["Km", 2],
                             Km.p=nls_coefs["Km", 4])
  } else {
    nls_params <- data.frame(Vmax=NA,
                             Vmax.std.err=NA,
                             Km=NA, 
                             Km.std.err=NA)
  }
  nls_params
}
