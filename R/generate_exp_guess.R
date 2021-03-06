##' Generate guesses for A and k in exponential fit
##' 
##' @description Fits a linear least-squares fit to log-transformed exponential data
##' @details When this fails, it is a good sign that the data don't fit an exponential model. Often this would be either because there are too few data points (1 or 0) or some of the data points are negative. In that case, the function returns NULL. (Although I'm not convinced that is the optimal return value). 
##' @param x vector of x values
##' @param y vector of y values
##' @export
#generate_exp_guess <- function(x, y) {
generate_exp_guess <- function(df, xcol, ycol) {

  # Strip down x and y to only values that you can take a log of
  df <- filter(df, (!!xcol) > 0 & (!!ycol) > 0)
  
  # Return NA if the data frame has less than 2 good values
  if(nrow(df) < 2) {
    # This usually is due to there being too many y values
    #   for which you can't take a log
    return(NA)
  } 
  
  # Make a column of the NATURAL log of y values
  df <- mutate(df, ln.ycol = log(!!ycol))
  
  # Create a linear model
  lin_mod <- tryCatch(
    #lm(log(!!ycol)~!!xcol, data=df),
    #lm(log.ycol ~ xcol, data = df),
    
    # This is an ugly way to do this!
    lm(unlist(select(df, ln.ycol)) ~ unlist(select(df, !!xcol))),
    error = function(err) {
      warning("error in using lm of log y ~ x to generate guesses for nls fit")
    },
    warning = function(warn) {
      warning("warning in using lm of log y ~ x to generate guesses for nls fit")
    }
  )
  
  ### NEEDS PROTECTION AGAINST BAD MODEL
  # Pull out the linear coefficients
  A_guess <- exp(coef(lin_mod)[1])
  k_guess <- coef(lin_mod)[2]
  
browser()
  # Return properly-named list of guesses to feed to nls
  guess_list=list("A"=A_guess, "k"=k_guess)
  
  guess_list
}