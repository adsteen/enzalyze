# Calculate NLS & return
# To do: import model as a parameter

safe_NLS <- function(df, xvar="conc", yvar="v0", form=NULL, guesses=NULL) {
  #modbrowser()
  # Create a default model based on Shane & Katherine's data
  if(is.null(form)) {
    mod <- formula(I(v0 ~ (Vmax * conc)/(Km + conc)))
  }
  
  xvals <- df[ , xvar]
  yvals <- df[ , yvar]
  
  if(is.null(guesses)) {
    # Need to include some code to ensure that the guesses have the same variables as the formula
    # warning("Dude you should really make a starting guess, but I'll try to make one for you")
    Km_guess <- mean(xvals, na.rm=TRUE)
    Vmax_guess <- max(xvals, na.rm=TRUE)
    guesses <- list(Km=Km_guess, Vmax=Vmax_guess)
  }
  
  mod <- tryCatch(
    nls(mod, df, start=guesses),
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      warning("This model threw an error")
      NULL
      })
  
  mod
}

