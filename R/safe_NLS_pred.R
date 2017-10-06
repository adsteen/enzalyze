##' Generates nls predictions
##' 
##' @param mod is a nls model (or any object for which there is a predict method)
##' @param domain of the data, as a two-element vector. Will project these onto a 100-element grid
##' @export
safe_NLS_pred <- function(mod, domain) {
  
  # Return a data frame of NAs if the input is NA
  if(class(mod) != "nls") {
    pred.vec <- rep(NA, length.out=100)
  } else {
    # Determine the domain over which to predict the nls object
    domain <- seq(from=min(domain), to=max(domain), length.out=100)
    
    # Put the domain into an APPROPRIATELY NAMED data frame
    pred_df <- data.frame(time=domain)
    # This gets me the name of the x variable but I don't know how to use it
    # names(mod$dataClasses)[1]
    
    # Predict the formula over the grid
    pred.vec <- predict(mod, newdata=pred_df)
  }
  
  # Put the predicted data into a data frame
  pred.df <- data.frame(time=domain, relative.ion.count=pred.vec)
  
  pred.df
}