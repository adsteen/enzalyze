library(shiny)
library(enzalyze)
library(lubridate)
library(plyr)


ui <- fluidPage( 
  
  titlePanel("File Upload"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "curve", label = "Calibration Curve"),
      helpText("Select the desired read.csv parameters"),
      checkboxInput(inputId = 'header2', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors2", label = "stringAsFactors", value = FALSE),
      radioButtons(inputId = 'sep2', label = 'Separator', 
                   choices = c(Comma=',', Semicolon=';', Tab='\t', Space=''),
                   selected = ',')
    ),
    mainPanel(
      uiOutput("concentration"),
      uiOutput("curvefluorescence"),
      uiOutput("cal")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "Data"),
      helpText("Select the desired read.csv parameters"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      radioButtons(inputId = 'sep', label = 'Separator', 
                   choices = c(Comma=',', Semicolon=';', Tab='\t', Space=''),
                   selected = ',')
    ),
    mainPanel(
      uiOutput("designvars"),
      uiOutput("timevar"),
      uiOutput("fluorescence"),
      uiOutput("uncal")
    )
  )
)

server <- function(input, output){
  
  # Read the Calibration Curve Data and extract the column names
  nms2 <- reactive({
    file1 <- input$curve
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, 
                        header = input$header2, 
                        stringsAsFactors = FALSE)
    nm <- names(d_uncal)
    nm
  })
  
  # Uses extracted column names from the calibration curve as options to select the concentration variable
  output$concentration <- renderUI({
    if(is.null(nms2()))
      return()
    else
      radioButtons("concentration.variable", "Select Concentration Variable", nms2())
  })
  
  # Uses extracted column names from the calibration curve as options to select the fluorescence variable
  output$curvefluorescence <- renderUI({
    if(is.null(nms2()))
      return()
    else
      radioButtons("fluorescence.curve", "Select Fluorescence Variable", nms2())
  })
  
  # used to carry out find_activty process for the calibration curve data
  # Returns head of data frame as checkpoint for this step
  data2 <- reactive({
    file1 <- input$curve
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header2, 
                        stringsAsFactors = FALSE)
    
    head(d_uncal)
  })
  
  ########ERROR: throwing an error when trying to get slope; dividing by a concentration of 0 
  slopecal <- reactive({
    file1 <- input$curve
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header2, 
                        stringsAsFactors = FALSE)
    cal_slope <- calib_slope(d = d_uncal, xvar = input$concentration.variable, 
                             yvar = input$fluorescence.curve)
    
    
    head(cal_slope)
  })
  
  # Creates tab about the calibration curve file
  output$file2 <- renderTable({
    if(is.null(data2())){return ()}
    input$curve
  })
  
  # Creates tab to display calibration curve data frame
  output$table2 <- renderTable({
    if(is.null(data2())){return ()}
    data2()
  })
  
  output$clslope <- renderTable({
    if(is.null(slopecal())){return ()}
    slopecal()
  })
  
  # Creates the UI output including all tabs about the calibration curve
  output$cal <- renderUI({
    if(is.null(data2()))
      return()
    else
      tabsetPanel(tabPanel("About file", tableOutput("file2")), 
                  tabPanel("Curve", tableOutput("table2")),
                  tabPanel("Slopes", tableOutput("clslope")))
  })
 
  ########################### Data ############################
  
  # Read the Raw Data and extract the names of the columns 
  nms <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, 
                        header = TRUE, stringsAsFactors = FALSE)
    nm <- names(d_uncal)
    nm
  })
  
  # Uses extracted column names from the raw data as options in a checkbox selector
  output$designvars <- renderUI({
    if(is.null(nms()))
      return()
    else
      checkboxGroupInput("design.variables", "Select Design Variables", nms())
  })
  
  # Uses extracted column names from the raw data as options to select the Time Variable
  output$timevar <- renderUI({
    if(is.null(nms()))
      return()
    else
      radioButtons("time.variable", "Select Time Variable", nms())
  })
  
  # Uses extracted column names from the raw data as options to select the fluorescence variable
  output$fluorescence <- renderUI({
    if(is.null(nms()))
      return()
    else
      radioButtons("fluorescence.variable", "Select Fluorescence Variable", nms())
  })
   
  # First step of find_activity; read .csv file
  # Returns head of data frame as checkpoint for this step
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header, 
                        stringsAsFactors = FALSE)
    # Save for later, for now assuming that data files are ideal
    #     d_uncal[ , input$fluorescence.variable] <- as.numeric(gsub(",", "", d_uncal[ ,input$fluorescence.variable]))
    
    
    head(d_uncal)
  })
  
  # Use this with data_plotr
  processed_data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header, 
                        stringsAsFactors = FALSE)
    # Save for later, for now assuming that data files are ideal
    d_uncal[ , input$fluorescence.variable] <- as.numeric(gsub(",", "", d_uncal[ ,input$fluorescence.variable]))
    the.date <- the_date(the.date = NULL)
    # Not sure i'll need this; just creates a character vector of the date & time
    #    When I add on ymd_hm it only returns the value in seconds
    #     d_uncal$Rtime <- paste(the.date, d_uncal[ ,input$time.variable])
    Rtime <- ymd_hm(paste(the.date, d_uncal[ , input$time.variable]))
    d_uncal$elapsed <- as.numeric(Rtime - min(Rtime))
    attr(d_uncal$elapsed, "units") <- "seconds"
    
    head(d_uncal)
  })
  
  lm_data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header, 
                        stringsAsFactors = FALSE)
    # Save for later, for now assuming that data files are ideal
    d_uncal[ , input$fluorescence.variable] <- as.numeric(gsub(",", "", d_uncal[ ,input$fluorescence.variable]))
    the.date <- the_date(the.date = NULL)
    # Not sure i'll need this; just creates a character vector of the date & time
    #    When I add on ymd_hm it only returns the value in seconds
    #     d_uncal$Rtime <- paste(the.date, d_uncal[ ,input$time.variable])
    Rtime <- ymd_hm(paste(the.date, d_uncal[ , input$time.variable]))
    d_uncal$elapsed <- as.numeric(Rtime - min(Rtime))
    attr(d_uncal$elapsed, "units") <- "seconds"
    
    lm_dframe <- uncalib_slope(d = d_uncal, id.var = input$design.variables,
                               time.var = "elapsed",
                               fluorescence = input$fluorescence.variable)
    
#     ###  Getting an error from Calib_Slope; first concentration is 0 and its trying to divide by zero
#     file2 <- input$curve
#     if(is.null(file2)){return()} 
#     d_cal <- read.csv(file = file2$datapath,
#                         header = input$header2, 
#                         stringsAsFactors = FALSE)
#     cal_slope <- calib_slope(d = d_cal, xvar = input$concentration.variable, 
#                              yvar = input$fluorescence.curve)
#     lm_dframe$v0 <- lm_dframe$slope / cal_slope
#     lm_dframe$v0.se <- lm_dframe$slope.se / cal_slope
#     ###
#     
    
    head(lm_dframe)
  })
  
  # Creates tab about the data file
  output$file <- renderTable({
    if(is.null(data())){return ()}
    input$data
  })
  
  # Creates tab to display dataframe
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  output$processed <- renderTable({
    if(is.null(processed_data())){return ()}
    processed_data()
  })
  
  output$lm_df <- renderTable({
    if(is.null(lm_data())){return ()}
    lm_data()
  })
  
  # Creates the UI output including all tabs about the raw data
  output$uncal <- renderUI({
    if(is.null(data()))
      return()
    else
      tabsetPanel(tabPanel("About file", tableOutput("file")),
                  tabPanel("Uploaded Data", tableOutput("table")),
                  tabPanel("Processed Data", tableOutput("processed")),
                  tabPanel("LM Stats", tableOutput("lm_df")))
  })
  
}

shinyApp(ui = ui, server = server)