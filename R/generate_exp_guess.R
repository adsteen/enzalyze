##' Generate guesses for A and k in exponential fit
##' 
##' @description Fits a linear least-squares fit to log-transformed exponential data
##' @details When this fails, it is a good sign that the data don't fit an exponential model. Often this would be either because there are too few data points (1 or 0) or some of the data points are negative. In that case, the function returns NULL. (Although I'm not convinced that is the optimal return value). 
##' @param x vector of x values
##' @param y vector of y values
##' @export
generate_exp_guess <- function(x, y) {
  
  # Strip down x and y to only values that you can take a log of
  bad.y <- (y <= 0) | (is.na(y)) | is.infinite(y)
  y <- y[!bad.y]
  x <- x[!bad.y]
  
  if(length(x) < 2) {
    # This usually is due to there being too many y values
    #   for which you can't take a log
    #   (e.g. negative numbers)
    # Could also happen if there are 
    return(NULL)
  } 
  
  # Take log values
  log.y <- log(y)
  
  # Create linear model of log-transformed data
  ### MUST WRAP THIS IN TRY.CATCH
  #lin.mod <- lm(log.y ~ x)
  lin.mod <- tryCatch(
    lm(log.y ~ x),
    error=function(err) {
      warning("lm error")
    },
    warning=function(warn) {
      warning("warning")
    }
    )
  
  # Pull out the linear coefficients
  A_guess <- exp(coef(lin.mod)[1])
  k_guess <- coef(lin.mod)[2]
  
  # Return properly-named list of guesses to feed to nls
  guess_list=list("A"=A_guess, "k"=k_guess)
  
  guess_list
}