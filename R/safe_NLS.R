##' Calculates exponential models safely
##' 
##' @details **Works only if x variable is named time and y variable is named relative.ion.count**. THis is because of the way generate_exp_guess works, must fix.
##' @param df A data frame
##' @param xvar The x variable for the exponential model
##' @param yvar The y variable for the exponential model
##' @export

safe_NLS <- function(df, xvar="time", yvar="relative.ion.count") {
  
  # To do: import model as a parameter
  # Intermediate step
  form <- formula(I(relative.ion.count ~ A * exp(-1*k*time)))
  
  ########
  # Test arguments
  ########
  
  
  ## KLUGE-Y FIX for the situation in which the df is too short to fit an nls
  ## GOTTA DO BETTER AT SOME POINT
  # test whether there are at least two valid points
  valid_df <- df[!is.na(df[ , xvar]) & !is.na(df[ , yvar]), ]
  if(nrow(valid_df) < 2) {
    # Return NA if the data frame doesn't have two valid points
    return(NA)
  }
  
  
  if(is.data.frame(df)) {
    # Test for the presence of xvar and yvar in df
    if(!(xvar %in% names(df))) {
      stop(paste("There is no column in df called ", xvar))
    }
    if(!(yvar %in% names(df))) {
      stop(paste("There is no column in df called ", yvar))
    }
  } else {
    stop("df must be a data frame, but the object you have passed is something else.")
  }
  
  # Turn xvar and yvar into vectors
  xvals <- df[ , xvar]
  yvals <- df[ , yvar]
  
  # Generate guesses for exponential fits
  guesses <- generate_exp_guess(xvals, yvals) #This is not right
  
  # Test whether generate_exp_guess failed
  if(is.null(guesses)) {
    return(NA)
  }
  
  # Determine domain for predictions
  dom <- c(min(xvals), max(xvals))
  
  # Generate a model, or return NA otherwise (should it be NULL?)
  mod <- tryCatch({
    mod <- nls(form, df, start=guesses)
    },
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      #warning("This model threw an error")
      NA
      })
  
  mod
}

