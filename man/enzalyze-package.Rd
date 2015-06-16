\name{enzalyze-package}
\alias{enzalyze-package}
\alias{enzalyze}
\docType{package}
\title{
A package for quantifying enzyme activity using fluorogenic substrates
~~ enzalyze ~~
}
\description{

~~ Within enzalyze are all the tools to calculate enzyme activities.  Beginning with melting data files, of various formats, into long form and reading them as a data frame.  Then by taking that data frame, extracting the statistics from a linear regression, and utilizing that data along with data from a calibration curve (also within the package) we can calculate true enzyme activity ~~

}
\details{
\tabular{ll}{
Package: \tab enzalyze\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-03\cr
License: \tab MIT: Copyright (C) 2013 Andrew D. Steen. Permission is hereby granted, 
  free of charge, to any person obtaining a copy of this software and associated
  documentation files (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions: The above copyright
  notice and this permission notice shall be included in all copies or substantial
  portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR 
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN 
  ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.\cr
}
~~ read_long : reads a raw dataset as a data frame, and omits any NA values ~~

~~ enzalyze_reform : Replaces numeric substrate labels with character sting names. Then calculates and inserts elapsed time values along with a character vector for the current date into new columns of the long-form data frame ~~

~~ uncalib_slope : split the data frame by specified id-variables and apply the lm_stats function to calculate linear regression statistics, then return the result in a data frame ~~

~~ lm_stats : create a linear model between a designated dependent and independent variable, from a long-form data frame, and returns the regression statistics (i.e slope, intercept, error, etc) ~~

~~ calib_slope : Calculate the slope of the calibration curve (in units of RFU per nM-AMC per L) by creating a linear model of the concentration and relative fluorescence, and extracting the slopes ~~

~~ find_activity : Calculates the true activity (in units of moles of substrate converted per unit time) by finding the slope of the uncalibrated & calibrated curves as a data frame, then dividing them to get a one-row data frame representing the activity ~~
}
\author{
Asst. Professor Andrew Steen
(with help from Josh Gurka)

Maintainer: Josh Gurka <jgurka@vols.utk.edu>
~~ Andrew Steen & Josh Gurka ~~
}
\references{
~~ Advanced R, Hadley Wickham ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ find_activity("uncalib_curve.csv", "calib_curve.csv",
         substrates = c("Arg-AMC", "Gly-AMC", "Leu-AMC", "Pyr-AMC", "GlyGlyArg-AMC"), 
         site.code = "testtest", print.plot = TRUE) ~~
}
