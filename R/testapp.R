library(shiny)


ui <- fluidPage( 
  
  titlePanel("File Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "Data"),
      helpText("Select the desired read.csv parameters"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE)
    ),
    mainPanel(
      # uiOutput("names"), DELETE
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
      checkboxInput(inputId = "stringAsFactors2", label = "stringAsFactors", value = FALSE)
    ),
    
    mainPanel(
      uiOutput("concentration.variable"),
      uiOutput("fluorescence.variable2")
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
  
  nms <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, 
                        header = input$header, 
                        stringsAsFactors = input$stringsAsFactors
                        )
    nm <- names(d_uncal)
    nm
  })
  
  nms2 <- reactive({
    file2 <- input$curve
    if(is.null(file2)){return()} 
    d_cal <- read.csv(file = file2$datapath, 
                      header = input$header2, 
                      stringsAsFactors = input$stringsAsFactors2
                      )
    nm2 <- names(d_cal)
    nm2
  })
  
  
#  Commented out until Im done and clean up old code
#   output$names <- renderPrint({
#     if(is.null(nms()))
#       return()
#     else
#       nms()
#     
#   })
  
  output$designvars <- renderUI({
    if(is.null(nms()))
      return()
    else
    checkboxGroupInput("design.variables", "Select Design Variables", nms())
  })
  
  output$timevar <- renderUI({
    if(is.null(nms()))
      return()
    else
      radioButtons("time.variable", "Select Time Variable", nms())
  })
  
  output$fluorescence <- renderUI({
    if(is.null(nms()))
      return()
    else
      radioButtons("fluorescence.variable", "Select Fluorescence Variable", nms())
    })
  
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath,
                        header = input$header, 
                        stringsAsFactors = input$stringsAsFactors
                        )
    head(d_uncal)
  })
  
  curve <- reactive({
    file2 <- input$curve
    if(is.null(file2)){return()} 
    d_cal <- read.csv(file = file2$datapath,
                      header = input$header2,
                      stringsAsFactors = input$stringAsFactors2)
    head(d_cal)
  })
  
  output$conc <- renderUI({
    if(is.null(nms2()))
      return()
    else
      radioButtons("concentration.variable", "Select Concentration Variable", nms2())
  })
  
  output$fluorescence2 <- renderUI({
    if(is.null(nms2()))
      return()
    else
      radioButtons("fluorescence.variable2", "Select Fluorescence Variable", nms2())
  })
}

shinyApp(ui = ui, server = server)