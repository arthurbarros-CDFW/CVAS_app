library(shiny)
library(tidyverse)
library(dplyr)


  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        # File input for multiple files
        fileInput("files", "Upload Data Files", accept = ".csv", multiple = TRUE),
        uiOutput("fileList"),  # Output for the list of uploaded files
        h4("Data List:"),
        verbatimTextOutput("dataListNames"),  # Output to display the names in dataList
        actionButton("cleanData", "Build Monthly Stats"),
        downloadButton("downloadData", "Download Monthly Stats")  # download button
      ),
      mainPanel(
        h4("Summary Output:"),
        dataTableOutput("cleanedData"),  # Output for cleaned data
        
      )
    )
  )
  
  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 100*1024^2) # sets max file size 10 100MB
    
    # Reactive values to store uploaded files, assigned datasets, and cleaned data
    uploadedFiles <- reactiveVal(list())
    datasets <- reactiveValues(tblCountSurv = NULL, tblCountDetail = NULL, tblGeoLoc = NULL,
                               tbl2ndCountSurv = NULL, tbl2ndCountDetail = NULL, tblIvDetail = NULL,
                               tblCatch = NULL, tblSpecies = NULL, tblIvSurv = NULL, tblMethod = NULL,
                               exp_lookup = NULL, edm_lookup = NULL)
    cleanedData <- reactiveVal(NULL)
    
    # Update the list of uploaded files
    observe({
      files <- input$files
      if (is.null(files)) return()
      
      uploadedFiles(files)
      
      # Read the data from the uploaded files into a list
      dataList <- lapply(1:nrow(files), function(i) {
        read.csv(files$datapath[i], header = TRUE)
      })
      
      # Assign names to dataList elements
      names(dataList) <- files$name
      
      # Display the names of datasets in dataList
      output$dataListNames <- renderPrint({
        names(dataList)
      })

      # Safely assign data to reactive variables based on file names
      datasets$tblCountSurv <- if(length(grep("tblCountSurv", files$name)) > 0) dataList[[grep("tblCountSurv", files$name)]] else NULL
      datasets$tblCountDetail <- if(length(grep("tblCountDetail", files$name)) > 0) dataList[[grep("tblCountDetail", files$name)]] else NULL
      datasets$tblGeoLoc <- if(length(grep("tblGeoLoc", files$name)) > 0) dataList[[grep("tblGeoLoc", files$name)]] else NULL
      datasets$tbl2ndCountSurv <- if(length(grep("tbl2ndCountSurv", files$name)) > 0) dataList[[grep("tbl2ndCountSurv", files$name)]] else NULL
      datasets$tbl2ndCountDetail <- if(length(grep("tbl2ndCountDetail", files$name)) > 0) dataList[[grep("tbl2ndCountDetail", files$name)]] else NULL
      datasets$tblIvDetail <- if(length(grep("tblIvDetail", files$name)) > 0) dataList[[grep("tblIvDetail", files$name)]] else NULL
      datasets$tblCatch <- if(length(grep("tblCatch", files$name)) > 0) dataList[[grep("tblCatch", files$name)]] else NULL
      datasets$tblSpecies <- if(length(grep("tblSpecies", files$name)) > 0) dataList[[grep("tblSpecies", files$name)]] else NULL
      datasets$tblIvSurv <- if(length(grep("tblIvSurv", files$name)) > 0) dataList[[grep("tblIvSurv", files$name)]] else NULL
      datasets$tblMethod <- if(length(grep("tblMethod", files$name)) > 0) dataList[[grep("tblMethod", files$name)]] else NULL
      datasets$exp_lookup <- if(length(grep("expansion_lookup", files$name)) > 0) dataList[[grep("expansion_lookup", files$name)]] else NULL
      datasets$edm_lookup <- if(length(grep("auto_edm_lookup", files$name)) > 0) dataList[[grep("auto_edm_lookup", files$name)]] else NULL
    })
    
    # Trigger the data cleaning script when the button is clicked
    observeEvent(input$cleanData, {
      # Ensure all datasets are available
      dataset_names <- c("tblCountSurv", "tblCountDetail", "tblGeoLoc", "tbl2ndCountSurv", "tbl2ndCountDetail",
                         "tblIvDetail", "tblCatch", "tblSpecies", "tblIvSurv", "tblMethod", "exp_lookup", "edm_lookup")
      
      # Check for missing datasets
      missing_datasets <- lapply(dataset_names, function(name) {
        if (is.null(datasets[[name]])) return(name) else return(NULL)
      })
      
      # Filter out the NULL values (those that are not missing)
      missing_datasets <- Filter(Negate(is.null), missing_datasets)
      
      # If any datasets are missing, show a notification and return
      if (length(missing_datasets) > 0) {
        showNotification(paste("The following datasets are missing:", paste(missing_datasets, collapse = ", ")), type = "error")
        return()
      }
      
      # fill datasets in the reactiveValues
      tblCountSurv <- datasets$tblCountSurv
      tblCountDetail <- datasets$tblCountDetail
      tblGeoLoc <- datasets$tblGeoLoc
      tbl2ndCountSurv <- datasets$tbl2ndCountSurv
      tbl2ndCountDetail <- datasets$tbl2ndCountDetail
      tblIvDetail <- datasets$tblIvDetail
      tblCatch <- datasets$tblCatch
      tblSpecies <- datasets$tblSpecies
      tblIvSurv <- datasets$tblIvSurv
      tblMethod <- datasets$tblMethod
      exp_lookup <- datasets$exp_lookup
      edm_lookup <- datasets$edm_lookup
      
      # Display a progress bar while running the sourced scripts
      withProgress(message = "Running data cleaning scripts...", value = 0, {
        # Increment progress for each script
        
        incProgress(0.25, detail = "Running app_data_prep.R")
        source("app_data_prep.R", local = TRUE)
        
        incProgress(0.25, detail = "Running app_expansions.R")
        source("app_expansions.R", local = TRUE)
        
        incProgress(0.25, detail = "Running app_summary.R")
        source("app_summary.R", local = TRUE)
        
        incProgress(0.25, detail = "Running app_statistics.R")
        source("app_statistics.R", local = TRUE)
      })
      
      # Assuming the cleaning script saves results as RDS files,
      # you could load one of those files and display it in the UI
      cleaned <- (readRDS("outputs/monthly_stats.rds"))
      cleanedData(cleaned)
    })
    
    # Display the cleaned data
    output$cleanedData <- renderDataTable({
      req(cleanedData())
      cleanedData()
    })
    
    # Define download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("CVAS_stats_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Save the cleaned data to a temporary file
        write.csv(cleanedData(), file,row.names = FALSE)
      }
    )
  }
  
  shinyApp(ui, server)
