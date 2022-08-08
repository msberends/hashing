# Licensed under GPL 2.0
# 
# August 2022
# Dr. Matthijs S. Berends
# 
# Simple Shiny app to encrypt laboratory data
# https://github.com/msberends/hashing

required_pkg <- c("shiny", "openssl", "DT", "openxlsx")
to_install <- required_pkg[!required_pkg %in% utils::installed.packages()]
if (length(to_install) > 0) {
  install.packages(to_install)
}

library(shiny)
library(openssl)
library(DT)
library(openxlsx)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hashing Laboratory Data with SHA-256"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("salt", "1. Fill in the 'salt', an institute-specific secret:", width = "100%"),
      hr(style = "border-top: 1px solid black;"),
      
      uiOutput("step2"),
      uiOutput("step3"),
      
      p(strong("DATA WILL NOT BE STORED")),
      p(strong("Open Source:")),
      p(strong(a("https://github.com/msberends/hashing"))),
      width = 4,
    ),
    
    mainPanel(
      uiOutput("mainpanel"),
      width = 8,
    )
  )
)

server <- function(input, output) {
  
  data_encrypt <- NULL
  
  output$step2 <- renderUI({
    if (trimws(input$salt) == "") {
      tagList()
    } else {
      tagList(
        radioButtons("datatype", "2. Give the values that must be hashed, using:",
                     choices = c("An Excel file" = "xlsx",
                                 "A CSV/TSV/TXT file" = "csv",
                                 "Values from clipboard" = "clip"),
                     selected = NULL),
        uiOutput("upload"),
        hr(style = "border-top: 1px solid black;")
      )
    }
  })
  output$step2 <- renderUI({
    if (trimws(input$salt) == "") {
      tagList()
    } else {
      tagList(
        radioButtons("datatype", "2. Give the values that must be hashed, using:",
                     choices = c("An Excel file" = "xlsx",
                                 "A CSV/TSV/TXT file" = "csv",
                                 "Values from clipboard" = "clip"),
                     selected = NULL),
        uiOutput("upload"),
        hr(style = "border-top: 1px solid black;")
      )
    }
  })
  
  output$step3 <- renderUI({
    if (is.null(input$start_hash)) {
      tagList()
    } else {
      tagList(
        p(("3. Download results:")),
        uiOutput("downloadbtn"),
        hr(style = "border-top: 2px solid black;")
      )
    }
  })
  
  output$upload <- renderUI({
    if (input$datatype == "xlsx") {
      tagList(
        fileInput("excel_file", "Select Excel file:", multiple = FALSE, accept = ".xlsx"),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "csv") {
      tagList(
        selectInput("delim", "Select the delimiter:",
                    choices = c("Comma: ," = ",",
                                "Semicolon: ;" = ";",
                                "Tab: \\t" = "\t"),
                    selected = ","),
        fileInput("csv_file", "Select CSV/TSV/TXT file:", multiple = FALSE, accept = ".csv"),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "clip") {
      tagList(
        selectInput("pasted_delim", "Select the delimiter (if pasting column data):",
                    choices = c("Nothing" = "",
                                "Comma: ," = ",",
                                "semicolon: ;" = ";",
                                "tab: \\t" = "\t"),
                    selected = ""),
        textAreaInput("pasted_values", "Paste values here:", height = "250px"),
        uiOutput("cols_to_hash")
      )
    }
  })
  
  
  output$cols_to_hash <- renderUI({
    if (input$datatype == "xlsx" && !is.null(input$excel_file)) {
      datafile <<- openxlsx::read.xlsx(input$excel_file$datapath)
      # remove the local file immediately
      unlink(input$excel_file$datapath)
    } else if (input$datatype == "csv" && !is.null(input$csv_file)) {
      datafile <<- readr::read_delim(input$csv_file$datapath, delim = input$delim)
      # remove the local file immediately
      unlink(input$csv_file$datapath)
    } else if (!is.null(input$pasted_values) && trimws(input$pasted_values) != "") {
      if (input$pasted_delim == "") {
        datafile <<- data.frame(x = input$pasted_values, stringsAsFactors = FALSE)
        return(tagList(
          checkboxInput("keep_originals", "Keep original columns", value = TRUE),
          actionButton("start_hash", "Click to Start Data Hashing")
        ))
      } else {
        datafile <<- readr::read_delim(input$pasted_values, delim = input$pasted_delim)
      }
    } else {
      datafile <<- NULL
    }
    if (is.null(datafile)) {
      tagList()
    } else {
      tagList(
        p("Uploaded data was deleted after import", style = "color: red;"),
        checkboxGroupInput("cols", "Select columns to hash:",
                           choices = colnames(datafile),
                           selected = 1),
        checkboxInput("keep_originals", "Keep original columns", value = FALSE),
        actionButton("start_hash", "Click to Start Data Hashing")
      )
    }
  })
  
  get_data <- eventReactive(input$start_hash,{
    as.data.frame(datafile, stringsAsFactors = FALSE)
  })
  get_data_encrypt <- eventReactive(input$start_hash,{
    data_encrypt
  })
  
  output$tableOut <- renderDT({
    df <- get_data()
    cols <- isolate(input$cols)
    keep_cols <- isolate(input$keep_originals)
    for (col in cols) {
      .new_hash_var <- as.character(openssl::sha256(df[, col, drop = TRUE], key = input$salt))
      pos <- which(colnames(df) == col)
      cols_before <- seq_len(pos)
      if (pos + 1 <= ncol(df)) {
         cols_after <- seq(from = pos + 1, to = ncol(df), by = 1)
      } else {
        cols_after <- character(0)
      }
      df <- data.frame(df[cols_before],
                       data.frame(.new_hash_var = .new_hash_var, stringsAsFactors = FALSE),
                       df[cols_after],
                       stringsAsFactors = FALSE)
      colnames(df)[colnames(df) == ".new_hash_var"] <- paste0(col, "_hash")
    }
    if (keep_cols == FALSE) {
      df <- df[, colnames(df)[!colnames(df) %in% cols], drop = FALSE]
    }
    data_encrypt <<- df
    datatable(
      df, 
      rownames = FALSE,
      extensions = c("Select"),
      selection = "none",
      options = 
        list(
          select = FALSE,
          dom = "Bfrtip",
          buttons = "")
    )
  }, server = FALSE)
  
  output$verbatimOut <- renderPrint({
    try(str(get_data_encrypt()), silent = TRUE)
  })
  
  output$downloadbtn <- renderUI({
    tagList(
      downloadButton("download_xlsx", "As Excel file"),
      downloadButton("download_csv", "As CSV file"),
      downloadButton("download_clip", "Copy to clipboard")
    )
  })
  
  output$mainpanel <- renderUI({
    if (trimws(input$salt) == "") {
      tagList() 
    } else {
      tagList(
        h3("Table output"),
        DTOutput("tableOut"),
        hr(),
        h3("Data Structure"),
        verbatimTextOutput("verbatimOut")
      )
    }
  })
  
  output$download_xlsx <- downloadHandler(
    filename = tempfile(paste0("hashed_", format(Sys.time(), "%Y%m%d_%H%M%S")), fileext = ".xlsx"),
    content = function(file) {
      openxlsx::write.xlsx(data_encrypt, file = file, asTable = FALSE, overwrite = TRUE)
    }
  )
  output$download_csv <- downloadHandler(
    filename = tempfile(paste0("hashed_", format(Sys.time(), "%Y%m%d_%H%M%S")), fileext = ".csv"),
    content = function(file) {
      readr::write_csv(data_encrypt, file = file, append = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
