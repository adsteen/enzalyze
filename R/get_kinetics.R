##' Get $V[max]$ and $K[m]$
##' 
##' @description Gets kinetic parameters from a list of nls fits; not terribly general at the moment.


get_kinetics <- function(x) {
  coefs <- tryCatch(
    summary(x)$coefficients,
    error=function(err) NA)
  
  if (is.na(coefs)) {
    kinetic_params <- rep(NA, 8)
  } else {
    kinetic_params <- c(coefs[1, ], coefs[2, ])
  }
  
  # Fix column names
  names(kinetic_params)[1:4] <- paste("Vmax", names(kinetic_params)[1:4])
  names(kinetic_params)[5:8] <- paste("Km", names(kinetic_params)[5:8])
  kinetic_params
}
