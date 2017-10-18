##' Used for diagnosing NSE error in generate_exp_guess.R


dummy_fun <- function(df, xcol, ycol) {
  
  #dummy_fun(mtcars, quo(cyl), quo(mpg)) works with this code:
  # fdf <- filter(df, (!!xcol) > 20)
  # fdf
  
  
  # but not this code
  mod <- lm(!!ycol ~ !!xcol, data=df)
  mod
  
  
}