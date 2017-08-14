# Calculate NLS & return
# To do: import model as a parameter

#safe_NLS <- function(df, xvar="conc", yvar="v0", form=NULL, guesses=NULL) {
safe_NLS <- function(df, xcol="conc", ycol="v0", form=NULL, guesses=NULL) {
  browser()
  # Create a default model based on Shane & Katherine's data
  if(is.null(form)) {
    form <- formula(I(v0 ~ (Vmax * conc)/(Km + conc)))
  }
  
  #xvals <- df[ , xvar]
  #yvals <- df[ , yvar]
  
  if(is.null(guesses)) {
    # Note: I could maybe get a better guess with a 2-tiered approach
    # First tier is LW-Burke approach, 2nd tier is this (or could use nls2 and try 'em both)
    
    # Need to include some code to ensure that the guesses have the same variables as the formula
    # Km_guess <- mean(xvals, na.rm=TRUE)
    Km_guess <- mean(df[ , xcol], na.rm=TRUE)
    #Vmax_guess <- max(yvals, na.rm=TRUE)
    Vmax_guess <- max(df[ , ycol], na.rm=TRUE)
    guesses <- list(Km=Km_guess, Vmax=Vmax_guess)
  }
  
  mod <- tryCatch(
    nls2(form, df, start=guesses),
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      warning("This model threw an error")
      NULL
      })
  
  mod
}

