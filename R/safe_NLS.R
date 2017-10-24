##' Calculates exponential models safely
##' 
##' @details This function generates guesses for an exponential mode in which 
##' @param df A data frame
##' @param xcol The x variable for the exponential model
##' @param ycol The y variable for the exponential model
##' @param form Formula to fit. At this point should be left as default
##' @param start_fun Funtion to generate starting guess. In general this will be specific to the formula to be fit.
##' @export

safe_NLS <- function(df, xcol=quo(time), ycol=quo(relative.ion.count), # These defaults make sense for metafluxr but not for a general case
                     form=relative.ion.count ~ A * exp(-1*k*time), 
                     start_fun=generate_exp_guess) {
  
  # Test that the columns supplied as xcol and ycol are in df
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
  
  
  # Test whether generate_exp_guess failed
  if(is.null(guesses)) {
    warning("Guessing function failed")
    return(NA) # Not really sure what the right thing to do in this case is
  }
  
  # Determine domain for predictions
  dom <- c(min(select(df, !!xcol)), max(select(df, !!xcol)))
  
  # Generate a model, or return NA otherwise (should it be NULL?)
  mod <- tryCatch(
    mod <- nls2::nls2(form, df, start=guesses),
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      warning(paste("The nonlinear fitting function nls2::nls2, using the formula", form, "threw the following error:\n", err))
      NA # need to wrap form in the function that collapses a character vector to a single vector element
      }
    )
  
  mod
}

