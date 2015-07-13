library(shiny)
library(enzalyze)
library(plyr)
library(lubridate)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  
  ## Raw Data upload display
  titlePanel("File Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "Data"),
      helpText("Select the desired read.csv parameters"),
      checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      radioButtons(inputId = 'sep', label = 'Separator', 
                   choices = c(Comma=',', Semicolon=';', Tab='\t', Space=''),
                   selected = ','),
      submitButton(text = "Print Data"),
      dataTableOutput("data")
      ),
    mainPanel(
      uiOutput("df")
    )
  ),
  
  ## Calibration curve upload display
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "curve", label = "Calibration Curve"),
      helpText("Select the desired read.csv parameters"),
      checkboxInput(inputId = 'header2', label = 'Header', value = FALSE),
      checkboxInput(inputId = "stringAsFactors2", label = "stringAsFactors", value = FALSE),
      radioButtons(inputId = 'sep2', label = 'Separator', 
                   choices = c(Comma=',', Semicolon=';', Tab='\t', Space=''),
                   selected = ','),
      submitButton(text = "Print Data")
    ),
    
    mainPanel(
      uiOutput("df2")
    )
    ),
  
  ## Create the space for plots; three tabs for the three plots
#   titlePanel("Plots"),
#   tabsetPanel(
#     tabPanel("Data", plotOutput("d")),
#     tabPanel("Calibration Curve", plotOutput("calcurve")),
#     tabPanel("Calibrated Activty", plotOutput("v0"))
#   ),
  
  mainPanel(
    uiOutput("plots")
  ),
  
  ## Space below plots, for parameter selection
  fluidRow(
    column(3,
           textInput("site.code", "Site Code"),
           dateInput("date", "Date"),
           textInput("substrate", "Substrates")), 
    column(4, offset = 1,
           selectInput("time.variable", "Time Variable", c("Elapsed" = "elapsed")),
           selectInput("fluorescence.variable", "Fluorescence Variable",
                       c("RFU" = "RFU", "FSU" = "FSU")),
           selectInput("concentration.variable", "Concentration Variable",
                       c("AMC.nM" = "AMC.nM",
                         "MUB.nM" = "MUB.nM"))),
    column(4, 
           checkboxGroupInput("design.variables", "Design Variables",
                                     c("Replicate" = "rep",
                                       "Treatment" = "treatment",
                                       "Substrate" = "substrate")),
           actionButton("plot", "Plot")
    )
  ),
  uiOutput("lm_dframe")
)


##################################### Server ###################################

server <- function(input, output){
  
  ## read data
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, sep = input$sep,
               header = input$header, stringsAsFactors = input$stringAsFactors)
    head(d_uncal)
  
  })
  
  # this reactive output contains the summary of the dataset and displays the summary in table format
  output$file <- renderTable({
    if(is.null(data())){return ()}
    input$data
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  ## This serves as a place holder message until a file is selected and read
  output$df <- renderUI({
    if(is.null(data()))
      h5("Go Vols, welcome to my shiny app", heigth=200, width=200)
    else
      tabsetPanel(tabPanel("About file", tableOutput("file")),
                  tabPanel("Data", tableOutput("table")))
  })
  
  ## read calibration curve
  curve <- reactive({
    file2 <- input$curve
    if(is.null(file2)){return()} 
    d2 <- read.csv(file = file2$datapath, sep = input$sep2, header = input$header2,
               stringsAsFactors = input$stringAsFactors2)
    head(d2)
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$file2 <- renderTable({
    if(is.null(curve())){return ()}
    input$curve
  })
  
  # This reactive output contains the dataset and displays the dataset in table format
  output$table2 <- renderTable({
    if(is.null(curve())){return ()}
    curve()
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$df2 <- renderUI({
    if(is.null(curve()))
      h5("...Nothing will work until you select some files")
    else
      tabsetPanel(tabPanel("About file", tableOutput("file2")), 
                  tabPanel("Curve", tableOutput("table2")))
  })
  
          ####################### Create Data Frames #########################
  
    # Create the data frame for the p_data plot
  ### possible error: the format that input$date sends the date into `enzalyze_reform`
  df_uncal <- reactive({
    dat <- input$data
    if(is.null(dat)){return()} 
    data_read <- read.csv(file = dat$datapath, sep = input$sep, header = input$header,
                   stringsAsFactors = input$stringAsFactors)
    dr_uncal <- enzalyze_reform(d = data_read, .labels = input$substrate, the.date = input$date)
  })
  
  
  # Create the data frame for the calibration curve
  d_cal <- reactive({
    file2 <- input$curve
    if(is.null(file2)){return()} 
    d2 <- read.csv(file = file2$datapath, sep = input$sep2, header = input$header2,
                   stringsAsFactors = input$stringAsFactors2)
    d2[ , "RFU"] <- as.numeric(gsub(",", "", d2[ ,"RFU"]))
    })
  
  
  # Create the data frame for v0 plot (full linear regression summary w/calibrated v0 and v0.se)
  # Potential error: how does shiny carry df_uncal() and d_cal()...how does it replicate code within
  lm_dframe <- reactive({
    lm_df <- uncalib_slope(d = df_uncal(), id.var = input$design.variables, 
                           time.var = input$time.variable,
                           fluorescence = input$fluorescence.variable)
    
    cal_slope <- calib_slope(d = d_cal(), xvar = input$concentration.variable, 
                             yvar = input$fluorescence.variable)
    
    lm_df$v0 <- lm_df$slope / cal_slope
    lm_df$v0.se <- lm_df$slope.se / cal_slope
    
  })

              ##################### Create Plots ########################
      
  # Create data plot, faceted by substrate
  ### possible error: df_uncal() is a function. Current thought is that it represnts all within
  ###                 the function, including the resulting data-frame.  NEED for the
  ###                 data frame to be inserted here for function to work...
  output$p_data <- renderPlot({
    input$plot
    if(input$plot == 0)
      return()
    else
    isolate(data_plotr(data = df_uncal(), datalabel = "Raw Data site ", 
                     site.code = input$site.code, time.variable = "elapsed", 
                     fluorescence.variable = "RFU",
                     shape = "treatment", colour = "rep", fill = "rep"))
  })
  
  # Create the plot for the calibration curve
  output$p_curve <- renderPlot({
    input$plot
    if(input$plot == 0)
      return()
    else
    isolate(curve_plotr(data = d_cal(), concentration.variable = input$concentration.variable, 
                      fluorescence.variable = input$fluorescence.variable, 
                      curvelabel = "Calibration Curve site ", site.code = input$site.code))
  })
  
  # Create the plot for calibrated v0
  output$p_activity <- renderPlot({
    input$plot
    if(input$plot == 0)
      return()
    else
    isolate(v0plotr(data = lm_dframe(), v0label = "Calibrated v0 site ", 
                  site.code = input$site.code))
  })
            ############### Render Plots and Final Data Frame ################### 
  
  # idea here is to dynamically render plots when parameters are entered
  output$plots <- renderUI({
    input$plot
    if(input$plot == 0)
      return()
    else
    isolate(tabsetPanel(tabPanel("Data", plotOutput("p_data")),
                tabPanel("Calibration Curve", plotOutput("p_curve")),
                tabPanel("Calibrated Activity", plotOutput("p_activity"))))
    
  })
  
  output$lm_dframe <- renderDataTable(
    lm_dframe()
    )
}

shinyApp(ui = ui, server = server)