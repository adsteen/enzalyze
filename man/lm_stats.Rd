\name{lm_stats}
\alias{lm_stats}
\title{Linear Modeling and Statistics}
\usage{
  lm_stats(d, xvar, yvar)
}
\arguments{
  \item{d}{Data frame containing at least two numeric
  columns}

  \item{xvar}{Numeric column to be used as the independant
  variable for the regression}

  \item{yvar}{Numeric column to be used as the dependant
  variable for the regression}
}
\value{
  Returns a one-row data frame containing (at present):
  slope, intercept, slope standard error, intercept
  standard error, p value, r-squared, and number of points.
}
\description{
  lm_stats is intended to take in a long-form data frame and return the statistics of a linear regression built around two numeric columns designated as either the dependent or independent variables. A specific example would be a long-form data frame with numeric values for relative fluorescence units (RFU) in one column, and elapsed time in another.  Set RFU as the independent variable and elapsed time as the dependent, then the returned slope of this linear regression is the activity in units of RFU/unit-time.
}
\details{
  Returns statistics for linear regressions (e.g. slope,
  intercept, error, etc)
}
\examples{
require(ggplot2)
lm_stats(mpg, xvar="cty", yvar="hwy")
}

