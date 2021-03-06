suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

source('helpers.R', local = FALSE)

ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'app.Rtext/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      radioButtons("filetype", "Download File type:",
                   choices = c("csv", "tsv", "psv")),
      tags$hr(),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
)

server <- function(input, output) {

  datasetInput <- reactive({
    
    inFile <- input$upload
    
    if (is.null(inFile)) return(NULL)
    
    v <- get_utm(inFile$datapath)
    return(v)
    
  })
  
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0('clean-', basename(input$upload$name))
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t", "psv" = "|")
      
      # Write to a file specified by the 'file' argument
      readr::write_delim(datasetInput(), path = file, delim = sep)
    }
  )

}

shinyApp(ui, server)
