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
    )
)


##################################### Server ###################################

server <- function(input, output){
  
  ## read data
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d <- read.csv(file = file1$datapath, sep = input$sep,
               header = input$header, stringsAsFactors = input$stringAsFactors)
    head(d)
  
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
      tabsetPanel(tabPanel("About file", tableOutput("file")), tabPanel("Data", tableOutput("table")))
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
      tabsetPanel(tabPanel("About file", tableOutput("file2")),tabPanel("Curve", tableOutput("table2")))
  })
}

shinyApp(ui = ui, server = server)