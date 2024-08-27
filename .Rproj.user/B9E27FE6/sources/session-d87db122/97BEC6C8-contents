library(shiny)

if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        # File input for multiple files
        fileInput("files", "Upload Data Files", accept = ".csv", multiple = TRUE),
        uiOutput("fileList"),  # Output for the list of uploaded files
        h4("Data List:"),
        verbatimTextOutput("dataListNames"),  # Output to display the names in dataList
        actionButton("cleanData", "Run Data Cleaning")
      ),
      mainPanel(
        h4("Summary Output:"),
        dataTableOutput("cleanedData")  # Output for cleaned data
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
      
      # Assign data to reactive variables based on file names
      datasets$tblCountSurv <- dataList[[grep("tblCountSurv", files$name)]]
      datasets$tblCountDetail <- dataList[[grep("tblCountDetail", files$name)]]
      datasets$tblGeoLoc <- dataList[[grep("tblGeoLoc", files$name)]]
      datasets$tbl2ndCountSurv <- dataList[[grep("tbl2ndCountSurv", files$name)]]
      datasets$tbl2ndCountDetail <- dataList[[grep("tbl2ndCountDetail", files$name)]]
      datasets$tblIvDetail <- dataList[[grep("tblIvDetail", files$name)]]
      datasets$tblCatch <- dataList[[grep("tblCatch", files$name)]]
      datasets$tblSpecies <- dataList[[grep("tblSpecies", files$name)]]
      datasets$tblIvSurv <- dataList[[grep("tblIvSurv", files$name)]]
      datasets$tblMethod <- dataList[[grep("tblMethod", files$name)]]
      datasets$exp_lookup<- dataList[[grep("expansion_lookup", files$name)]]
      datasets$edm_lookup<- dataList[[grep("auto_edm_lookup", files$name)]]
    })
    
    # Trigger the data cleaning script when the button is clicked
    observeEvent(input$cleanData, {
      # Ensure all datasets are available
      if (is.null(datasets$tblCountSurv) || is.null(datasets$tblCountDetail) || is.null(datasets$tblGeoLoc) ||
          is.null(datasets$tbl2ndCountSurv) || is.null(datasets$tbl2ndCountDetail) || is.null(datasets$tblIvDetail) ||
          is.null(datasets$tblCatch) || is.null(datasets$tblSpecies) || is.null(datasets$tblIvSurv) || is.null(datasets$tblMethod) || is.null(datasets$exp_lookup) || is.null(datasets$edm_lookup)) {
        showNotification("Some required datasets are missing.", type = "error")
        return()
      }
      
      # Source the cleaning script and run it using the datasets in the reactiveValues
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
      
      # Display a progress bar during the data cleaning process
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
  }
  
  shinyApp(ui, server)
}