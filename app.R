# Licensed under GPL 2.0
# 
# August 2022
# Dr. Matthijs S. Berends
# 
# Simple Shiny app to encrypt laboratory data
# https://github.com/msberends/hashing

library(shiny)
library(openssl)
library(DT)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hashing Laboratory Data with SHA-256"),
  
  sidebarLayout(
    sidebarPanel(
      p("1. Fill in the salt, a institute-specific secret."),
      textInput("salt", "Salt to use for hashing:", width = "100%"),
      hr(style = "border-top: 1px solid black;"),
      
      p("2. Give the values that must be hashed, using:"),
      
      fileInput("excel", "2a. An Excel file:", multiple = FALSE, accept = ".xlsx"),
      textInput("range", "Excel Range (such as 'Sheet1$A:A'):"),
      
      fileInput("csv", "2b. An Excel file:", multiple = FALSE, accept = ".csv"),
      textInput("delim", "Delimiter (comma, semicolon, tab):", value = ","),
      
      textAreaInput("values", "2c. Pasted values from clipboard:", height = "150px"),
      hr(style = "border-top: 1px solid grey;"),
      
      
      hr(style = "border-top: 2px solid black;"),
      p(strong("DATA WILL NOT BE STORED")),
      p(strong("Open Source:")),
      p(strong(a("https://github.com/msberends/hashing"))),
      width = 4,
    ),
    
    mainPanel(
      uiOutput("downloadbtn"),
      hr(),
      h3("Table output"),
      DTOutput("tableOut"),
      hr(),
      h3("Raw output"),
      verbatimTextOutput("verbatimOut"),
      width = 8,
    )
  )
)

server <- function(input, output) {
  
  output$tableOut <- renderDT({
    values <- strsplit(input$values, "\n", fixed = TRUE)[[1]]
    out <- data.frame(input = values,
                      hash = as.character(openssl::sha256(values, key = input$salt)),
                      stringsAsFactors = FALSE)
    if (trimws(input$salt) == "") {
      datatable(data.frame(ERROR = "FILL IN SALT FIRST"))
    } else {
      datatable(
        out, 
        rownames = FALSE,
        extensions = c("Buttons", "Select"),
        selection = 'none',
        options = 
          list(
            select = FALSE,
            dom = "Bfrtip",
            buttons = list(
              list(
                extend = "copy",
                text = 'Copy result to clipboard'
              )
            )
          )
      )
    }
  }, server = FALSE)
  
  output$verbatimOut <- renderPrint({
    if (trimws(input$salt) == "") {
      "ERROR - fill in salt first"
    } else {
      values <- strsplit(input$values, "\n", fixed = TRUE)[[1]]
      as.character(openssl::sha256(values, key = input$salt))
    }
  })
  
  output$downloadbtn <- renderUI({
    tagList(
      actionButton("download_xlsx", "Download result as Excel file"),
      actionButton("download_csv", "Download result as CSV file")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
