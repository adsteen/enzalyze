library(shiny)


ui <- fluidPage( 
  
  titlePanel("File Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "Data")
    ),
    mainPanel(
      uiOutput("names"),
      uiOutput("designvars")
    )
  )
)

server <- function(input, output){
  
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
                        header = TRUE, stringsAsFactors = FALSE)
    nm <- names(d_uncal)
    nm
  })
  
  output$names <- renderPrint({
    if(is.null(nms()))
      return()
    else
      nms()
    
  })
  
  output$designvars <- renderUI({
    if(is.null(nms()))
      return()
    else
    checkboxGroupInput("design.variables", "Select Design Variables", nms())
  })
}

shinyApp(ui = ui, server = server)