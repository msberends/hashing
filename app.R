# Licensed under GPL 2.0
# 
# August 2022
# Dr. Matthijs S. Berends
# 
# Simple Shiny app to encrypt data for safe, anonymised data transfer
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
  titlePanel("Hashing Data with SHA-256 and HMAC"),
  p(HTML("This is a simple Shiny app to encrypt data for safe, anonymised data transfer. The app uses the <code>openssl</code> package to provide <a href='https://en.wikipedia.org/wiki/SHA-2'>SHA-2 hashing</a> using <a href='https://en.wikipedia.org/wiki/HMAC'>HMAC</a> for adding a key or 'salt' (required for this app) and a bit size of 256 bits.")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("key", "1. Fill in a self-chosen key, e.g. an institute-specific secret:", width = "100%"),
      p("Note: this key must be kept secret from the data-receiving partner!", style = "font-style: italic;"),
      hr(style = "border-top: 1px solid black;"),
      
      uiOutput("step2"),
      uiOutput("step3"),
      
      p(strong("* DATA WILL NOT BE STORED *")),
      width = 5,
    ),
    
    mainPanel(
      uiOutput("mainpanel"),
      width = 7,
    )
  ),
  
  p(HTML("Read more here: <a href='https://github.com/msberends/hashing/blob/main/README.md'>https://github.com/msberends/hashing</a>")),
  p(HTML("<strong>Author:</strong> Dr. Matthijs Berends<br><strong>Copyright:</strong> this is free and open-source software, licensed under GNU GPL-2.0<br><strong>Source:</strong> <a href='https://github.com/msberends/hashing/blob/main/app.R'>https://github.com/msberends/hashing/blob/main/app.R</a>")),
  p("Supported file types: MS Excel, SAS, SPSS, Stata, flat files (CSV, TSV, TXT), and clipboard data"),
)

server <- function(input, output) {
  
  data_encrypt <- NULL
  
  output$step2 <- renderUI({
    if (trimws(input$key) == "") {
      tagList()
    } else {
      tagList(
        radioButtons("datatype", "2. Give the values that must be hashed, using:",
                     choices = c("An Excel file" = "xlsx",
                                 "A CSV / TSV / TXT file" = "csv",
                                 "An SAS file" = "sas",
                                 "An SPSS file" = "spss",
                                 "A Stata file" = "stata",
                                 "Clipboard" = "clip"),
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
        p("This uses the 'openxlsx' R package.", style = "font-style: italic;"),
        fileInput("excel_file", "Select Excel file:", multiple = FALSE, accept = c(".xlsx", ".xlsm")),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "sas") {
      tagList(
        p("This uses the 'foreign' R package, which is part of base R.", style = "font-style: italic;"),
        fileInput("sas_file", "Select SAS file:", multiple = FALSE),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "spss") {
      tagList(
        p("This uses the 'foreign' R package, which is part of base R.", style = "font-style: italic;"),
        fileInput("spss_file", "Select SPSS file:", multiple = FALSE, accept = ".sav"),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "stata") {
      tagList(
        p("This uses the 'foreign' R package, which is part of base R.", style = "font-style: italic;"),
        fileInput("stata_file", "Select Stata file:", multiple = FALSE, accept = ".dta"),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "csv") {
      tagList(
        selectInput("delim", "Select the delimiter:",
                    choices = c("Comma: ," = ",",
                                "Semicolon: ;" = ";",
                                "Tab: \\t" = "\t"),
                    selected = ","),
        fileInput("csv_file", "Select CSV/TSV/TXT file:", multiple = FALSE, accept = c(".csv", ".txt", "tsv")),
        uiOutput("cols_to_hash")
      )
    } else if (input$datatype == "clip") {
      tagList(
        selectInput("pasted_delim", "Select the delimiter (if pasting column data):",
                    choices = c("Nothing" = "nothing",
                                "Comma: ," = ",",
                                "semicolon: ;" = ";",
                                "tab: \\t" = "\t"),
                    selected = ""),
        textAreaInput("pasted_values", "Paste the values here:", height = "250px"),
        uiOutput("cols_to_hash")
      )
    }
  })
  
  
  output$cols_to_hash <- renderUI({
    # Excel
    if (input$datatype == "xlsx" && !is.null(input$excel_file)) {
      datafile <<- openxlsx::read.xlsx(input$excel_file$datapath)
      # remove the local file immediately
      unlink(input$excel_file$datapath)
      
      # SAS
    } else if (input$datatype == "sas" && !is.null(input$sas_file)) {
      datafile <<- foreign::read.ssd(input$sas_file$datapath)
      # remove the local file immediately
      unlink(input$sas_file$datapath)
      
      # SPSS
    } else if (input$datatype == "spss" && !is.null(input$spss_file)) {
      datafile <<- foreign::read.spss(input$spss_file$datapath)
      # remove the local file immediately
      unlink(input$spss_file$datapath)
      
      # Stata
    } else if (input$datatype == "stata" && !is.null(input$stata_file)) {
      datafile <<- foreign::read.dta(input$stata_file$datapath)
      # remove the local file immediately
      unlink(input$stata_file$datapath)
      
      # CSV / TSV / TXT
    } else if (input$datatype == "csv" && !is.null(input$csv_file)) {
      datafile <<- read.delim(input$csv_file$datapath, sep = input$delim, stringsAsFactors = FALSE)
      # remove the local file immediately
      unlink(input$csv_file$datapath)
      
      # Clipboard
    } else if (!is.null(input$pasted_values) && trimws(input$pasted_values) != "") {
      values <- strsplit(input$pasted_values, "\n", fixed = TRUE)[[1]]
      if (input$pasted_delim == "nothing") {
        datafile <<- data.frame(x = values,
                                stringsAsFactors = FALSE)
        return(tagList(
          actionButton("start_hash", "Click to Start Data Hashing")
        ))
      } else {
        datafile <<- read.delim(text = values, sep = input$pasted_delim, stringsAsFactors = FALSE)
      }
    } else {
      datafile <<- NULL
    }
    if (is.null(datafile)) {
      tagList()
    } else {
      tagList(
        p("Note: uploaded data file was already deleted after import. By closing this window, you will lose your data.", style = "color: red;"),
        checkboxGroupInput("cols", "Select columns to hash:",
                           choices = colnames(datafile),
                           inline = TRUE),
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
    if (input$pasted_delim == "nothing") {
      cols <- colnames(df)
      keep_cols <- FALSE
    }
    for (col in cols) {
      .new_hash_var <- as.character(openssl::sha256(df[, col, drop = TRUE], key = input$key))
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
                       stringsAsFactors = FALSE,
                       check.names = FALSE,
                       fix.empty.names = FALSE)
      colnames(df)[colnames(df) == ".new_hash_var"] <- paste0(col, "_hash")
    }
    if (isFALSE(keep_cols)) {
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
      downloadButton("download_xlsx", "As Excel file", width = "100%"),
      downloadButton("download_csv", "As CSV file", width = "100%"),
      downloadButton("download_clip", "Copy to clipboard", width = "100%")
    )
  })
  
  output$mainpanel <- renderUI({
    if (trimws(input$key) == "") {
      tagList() 
    } else {
      tagList(
        h3("Example Table Output"),
        DTOutput("tableOut"),
        hr(),
        h3("Example Data Structure"),
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
