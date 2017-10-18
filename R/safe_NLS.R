##' Calculates exponential models safely
##' 
##' @details **Works only if x variable is named time and y variable is named relative.ion.count**. THis is because of the way generate_exp_guess works, must fix.
##' @param df A data frame
##' @param xcol The x variable for the exponential model
##' @param ycol The y variable for the exponential model
##' @param form Formula to fit. At this point should be left as default
##' @param start_fun Funtion to generate starting guess. In general this will be specific to the formula to be fit.
##' @export

safe_NLS <- function(df, xcol=quo(time), ycol=quo(relative.ion.count), # These defaults make sense for metafluxr but not for a general case
                     form=relative.ion.count ~ A * exp(-1*k*time), 
                     start_fun=generate_exp_guess) {
  
  # Old: xvar="time", yvar="relative.ion.count", 
  
  # # To do: import model as a parameter
  # # Intermediate step
  # if(is.null(form)) {
  #   form <- formula(I(relative.ion.count ~ A * exp(-1*k*time)))
  # }
  
  
  ####
  # Rewrite using nonstandard evaluation
  ####
  
  ### Test input arguments first - not totally sure how to do that with expressions
  # Test
  #browser()
  if(is.data.frame(df)) {
    # Test that the specified columns are present
    tryCatch(
      select(df, !!xcol, !!ycol),
      error = function(err) {
        stop("One of the specified columns is missing from the data frame")
        NA # Do I really need to return NA here? I think there is no point
      }
    )
    } else {
    error(paste("The object supplied as argument df is of class", class(df), ", but it should be a data frame"))
  }

  
  # Generate guesses for exponential fits
  guesses <- start_fun(df, xcol, ycol) 
  
  
  
  ########
  # Test arguments
  ########
  
  
  ## KLUGE-Y FIX for the situation in which the df is too short to fit an nls
  ## GOTTA DO BETTER AT SOME POINT
  # test whether there are at least two valid points
  # valid_df <- df[!is.na(df[ , xvar]) & !is.na(df[ , yvar]), ]
  # if(nrow(valid_df) < 2) {
  #   # Return NA if the data frame doesn't have two valid points
  #   return(NA)
  # }
  
  
  # if(is.data.frame(df)) {
  #   # Test for the presence of xvar and yvar in df
  #   if(!(xvar %in% names(df))) {
  #     stop(paste("There is no column in df called ", xvar))
  #   }
  #   if(!(yvar %in% names(df))) {
  #     stop(paste("There is no column in df called ", yvar))
  #   }
  # } else {
  #   stop("df must be a data frame, but the object you have passed is something else.")
  # }
  # 
  # # Turn xvar and yvar into vectors
  # xvals <- df[ , xvar]
  # yvals <- df[ , yvar]
  
  browser()
  
  # Test whether generate_exp_guess failed
  if(is.null(guesses)) {
    return(NA) 
  }
  
  # Determine domain for predictions
  dom <- c(min(select(df, !!xcol)), max(select(df, !!xcol)))
  
  # Generate a model, or return NA otherwise (should it be NULL?)
  mod <- tryCatch({
    mod <- nls2::nls2(!!form, df, start=guesses)
    },
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      #warning("This model threw an error")
      NA
      })
  
  mod
}

