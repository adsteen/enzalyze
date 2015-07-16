library(shiny)

ui <- fluidPage( 
  
  titlePanel("File Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "Data"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value = FALSE),
      radioButtons(inputId = 'sep', label = 'Separator', 
                   choices = c(Comma=',', Semicolon=';', Tab='\t', Space=''),
                   selected = ','),
      submitButton(text = "Print Data"),
      dataTableOutput("data")
    ),
    mainPanel(
      uiOutput("df"),
      uiOutput("names")
    )
  )
  
)

server <- function(input, output){
  
  data <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, sep = input$sep,
                        header = input$header, stringsAsFactors = input$stringAsFactors)
    head(d_uncal)
    
  })
  
  output$file <- renderTable({
    if(is.null(data())){return ()}
    input$data
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  nms <- reactive({
    file1 <- input$data
    if(is.null(file1)){return()} 
    d_uncal <- read.csv(file = file1$datapath, sep = input$sep,
                        header = input$header, stringsAsFactors = input$stringAsFactors)
    nms <- names(d_uncal)
    nms
  })
  
  output$df <- renderUI({
    if(is.null(data()))
      return()
    else
      tableOutput("table")
  })
  
  output$names <- renderPrint({
    if(is.null(nms()))
      return()
    else
      nms()
      
  })
  
}

shinyApp(ui = ui, server = server)