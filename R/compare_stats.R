##' Compare statistics of regressions from multiple sites
##' 
##' A function to return the linear regression statistics and plots of multiple tests at once
##' @param
##' @return 
##' @export

compare_stats <- function(site1.uncal, site2.uncal = NULL, site1.cal, site2.cal = NULL,
                          site1.code, site2.code = NULL, substrates, the.date = NULL,
                          design.variables = c("rep", "treatment", "substrate"), 
                          time.variable = "elapsed", fluorescence.variable = "RFU",
                          concentration.variable = "conc.AMC.nM", print.plot = FALSE,
                          save.plot = FALSE, plot.filename = NULL, save.datafile = FALSE,
                          datafile.filename = NULL){
  
  site_1 <- find_activity(site1.uncal, site1.cal, site.code = site1.code, 
                          substrates = substrates, .the.date = the.date, 
                          design.variables = design.variables, time.variable = time.variable, 
                          fluorescence.variable = fluorescence.variable,
                          concentration.variable = concentration.variable, 
                          print.plot = print.plot, save.plot = save.plot, 
                          plot.filename = plot.filename, save.datafile = save.datafile, 
                          datafile.filename = datafile.filename)
  
  site_2 <- find_activity(site1.uncal, site1.cal, site.code = site1.code, 
                          substrates = substrates, .the.date = the.date, 
                          design.variables = design.variables, time.variable = time.variable, 
                          fluorescence.variable = fluorescence.variable,
                          concentration.variable = concentration.variable, 
                          print.plot = print.plot, save.plot = save.plot, 
                          plot.filename = plot.filename, save.datafile = save.datafile, 
                          datafile.filename = datafile.filename)
  
  site_1
  
  site_2
}