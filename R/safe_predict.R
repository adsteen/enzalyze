##' Safely makes predictions of y values of nls fits

safe_predict <- function(l, domain) {
  # Domain should be a vector
  df <- data.frame(conc=domain)
  
  # Fit the data frame
  if(class(l)=="nls") {
    preds <- predict(l, newdata=df)
  } else {
    preds <- rep(NA, length(domain))
  }
  
  # Could add arguments for name fo conc and v0
  data.frame(conc=domain, v0=preds)
}