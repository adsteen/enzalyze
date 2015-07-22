library(shiny)


ui <- fluidPage( 
  
  titlePanel("File Upload"),
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
      # uiOutput("names"), DELETE
      uiOutput("uncal"),
      uiOutput("designvars"),
      uiOutput("timevar"),
      uiOutput("fluorescence")
    )
  ),
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
  )
)

server <- function(input, output){
  
# Commented out until im sure I dont need this here
#   data <- reactive({
#     file1 <- input$data
#     if(is.null(file1)){return()} 
#     d_uncal <- read.csv(file = file1$datapath,
#                         header = TRUE, stringsAsFactors = FALSE)
#     head(d_uncal)
#   })
  
  # Read the Raw Data and extract the names of the columns 
  nms <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, 
                        header = TRUE, stringsAsFactors = FALSE)
    nm <- names(d_uncal)
    nm
  })
  
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
  
#  Commented out until Im done and clean up old code
#   output$names <- renderPrint({
#     if(is.null(nms()))
#       return()
#     else
#       nms()
#     
#   })
  
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
  
  # First step of find_activity; read .csv file
  # Returns head of data frame as checkpoint for this step
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header, 
                        stringsAsFactors = FALSE)
    
    head(d_uncal)
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
  
  # Creates the UI output including all tabs about the raw data
  output$uncal <- renderUI({
    if(is.null(data()))
      return()
    else
      tabsetPanel(tabPanel("About file", tableOutput("file")),
                  tabPanel("Uploaded Data", tableOutput("table")))
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
  
  # Creates the UI output including all tabs about the calibration curve
  output$cal <- renderUI({
    if(is.null(data2()))
      return()
    else
      tabsetPanel(tabPanel("About file", tableOutput("file2")), 
                  tabPanel("Curve", tableOutput("table2")))
  })
  
}

shinyApp(ui = ui, server = server)