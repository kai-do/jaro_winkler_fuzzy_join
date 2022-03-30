require("tidyverse")
require("RecordLinkage")
require("sqldf")
require("shiny")
require("DT")
require("dplyr")

source("jaro_winkler_fuzzy_joiner.R")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      tags$hr(),
      
      
      
      selectInput(
        "columns",
        "Select Column",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      selectInput(
        "algorithm",
        "Fuzzy Select Algorithm",
        choices = c("Jaro Winkler"),
        selected = c("Jaro Winkler"),
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      numericInput(
        "precision",
        "Precision",
        0.93,
        min = 0,
        max = 1,
        step = 0.01,
        width = NULL
      ),
      
      numericInput(
        "row_limit",
        "Top Rows to Process",
        100,
        min = 100,
        max = 1000,
        step = 100,
        width = NULL
      ),
      
      actionButton("process", "Remove Uniques")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      dataTableOutput("contents"),
      
      textOutput("unique"),
      
      textOutput("row_limit"),
      
      dataTableOutput("test_list")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  df_reactive <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  list_reactive <- reactive({
    req(input$columns)
    
    df_reactive() %>%
      select(input$columns)
  })
  
  #fuzzy_reactive <- reactive({
  #  fuzzy_join(list = list_reactive(), precision = input$precision, row_limit = input$row_limit)
  #})
  
  observeEvent(df_reactive(), {
    updateSelectInput(session = session, inputId = "columns", choices = colnames(df_reactive()))
  })
  
  output$contents <- renderDataTable({
    as.data.frame(df_reactive())
  })
  
  #fuzzy_output <- eventReactive(input$process, {
  fuzzy_output <- eventReactive(input$process, {
    list <- list_reactive()
    precision <- input$precision
    row_limit <- input$row_limit
    
    matches <- c()
    names <- list()
    matched_names <- list()
    unique <- data.frame()
    start_time <- Sys.time()
    
    i <- 1
    if (is.null(row_limit)) {
      max_i <- length(list)
    } else {
      max_i <- row_limit
    }
    
    #withProgress(message = "Processing...", value = 0, {
    
      for (i in i:max_i) {
        if (i == 1) {
          loop_df <- as.data.frame(cbind(name = list[], match_score = jarowinkler(list[i],list[1:max_i]))) 
        } else if (i <= max_i) {
          loop_df <- as.data.frame(cbind(name = loop_df$name[], match_score = jarowinkler(loop_df$name[i],loop_df$name[1:max_i])))
        }
        
        if (i <= max_i) {
          matches <- loop_df %>%
            filter(match_score > precision)
          names[[i]] <- paste(matches$name[[1]],"-",format(matches$match_score[[1]], digits = 3))
          matched_names[[i]] <- paste(matches$name,"-",format(matches$match_score, digits = 3))
          
          loop_df <- loop_df[!(loop_df$name %in% matches),]
          
          if (is.null(row_limit)) {
            max_i <- nrow(loop_df)
            max_list_length <- nrow(df)
          } else if (!is.null(row_limit) & i == 1) {
            max_list_length <- row_limit 
          }
          
          if (i %% 10 == 0 | i == max_i) {
            #cat("\r===============================================================================================================")
            #cat(paste0("\r======| Rows Remaining: ", (max_i - i), " | Unique Matches: ", max_list_length - (max_list_length - length(unique(names))), " of ", max_list_length, " | Progress: ", format((i / max_i) * 100, digits = 3), "% | Time Elapsed: ", format(difftime(Sys.time(), start_time, units = "mins"), digits = 3), " |"))
          #incProgress((i / max_i) * 100)
          }
        }
      }
    #})
    
    match_df <- data.frame(cbind(names = names, matched_names = matched_names))
    unique_matches <- length(unique(match_df$names))
    
    #return(list(df = match_df, unique_matches = unique_matches, precision = precision))
    return(unique_matches = unique_matches, precision = precision))
    })
  
  unique_output <- eventReactive(input$process, {
    output_test_list <- list(list = list_reactive(), precision = input$precision, row_limit = input$row_limit)
  })
  
  output$unique <- renderText({
    #unique_output()$unique_matches
    req(fuzzy_output())
    
    fuzzy_output()$unique_matches
  })
  
  output$row_limit <- renderText({
    #unique_output()$unique_matches
    unique_output()$row_limit
  })
  
  output$test_list <- renderDataTable({
    as.data.frame(unique_output()$list)
  })
}
# Run the app ----
shinyApp(ui, server)
