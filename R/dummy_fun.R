##' Used for diagnosing NSE error in generate_exp_guess.R


dummy_fun <- function(df, xcol, ycol) {
  
  #dummy_fun(mtcars, quo(cyl), quo(mpg)) works with this code:
  # fdf <- filter(df, (!!xcol) > 20)
  # fdf
  
  
  # but not this code
  mod <- lm(!!ycol ~ !!xcol, data=df)
  mod
  
  
}


# SO answer
my_lm <- function(df, xcol, ycol) {
  form <- as.formula(paste(ycol, " ~ ", xcol)[2])
  my_lm <- lm(form, data=df)
  my_lm
}