#Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(httr)
library(jsonlite)
library(DT)
library(crosstalk)


# APP SETTINGS ---------------------------------------------------------------- 

if(!file.exists("settings.R")){
  source("settings-default.R")} else {
    source("settings.R")} # To customise settings, please create settings.R

# LOAD GRAPHQL ----------------------------------------------------------------

source("graphql_functions/getToken.R")  # Added for manual login
source("graphql_functions/getPatientIds.R")
source("graphql_functions/getMultiplePatientReports.R")


# LOAD UTILITY ----------------------------------------------------------------

source("utility_functions/simplifyPatRep.R")
source("utility_functions/simplifyMultPatRep.R")
source("utility_functions/calculateScales.R")
source("utility_functions/applyCutOffs.R")
source("utility_functions/severityPlot.R")
source("utility_functions/inactivity.R")
source("utility_functions/interpretTable.R")
source("utility_functions/checkGraphqlResponse.R")
source("utility_functions/patientInfoTable.R")
source("utility_functions/extract_cutoffs.R")
source("utility_functions/groupCutoffs.R")
source("utility_functions/printItemTable.R")

inactivity = inactivity(timeoutSeconds)

# LOAD TRANSLATION MATRIX -----------------------------------------------------

transMatrix = data.frame(fread("www/transMatrix.csv"), row.names = "Key")

# USER INTERFACE ##------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
                    title="MHIRA",
                    
                    # HEADER ------------------------------------------------------------------ 
                    dashboardHeader(
                      title = tags$a(href='http://mhira-project.org',
                                     tags$img(src='mhira_logo.png', height='50', width='150'),
                                     'MHIRA')
                    ),
                    
                    
                    # SIDEBAR ------------------------------------------------------------------
                    dashboardSidebar(
                      width = 250,
                      collapsed = FALSE,
                      tags$script(inactivity), # For timeout
                      
                      tags$script(HTML( # This javascript code gets data from localStorage of the browser
                        "$(document).on('shiny:connected', function() {
            const LS = window.localStorage.getItem('auth_app_token');
            Shiny.setInputValue('accessToken', LS);
            const CL = window.localStorage.getItem('currentLang');
            Shiny.setInputValue('currentLang', CL);
            });"
                      )),
                      
                      br(),
                      
                      # Add login status indicator
                      uiOutput("loginStatus"),
                      
                      # Add manual login panel for debugging
                      conditionalPanel(
                        condition = "output.isLoggedIn != true",
                        h4("Debug Login"),
                        textInput("username", "Username"),
                        passwordInput("password", "Password"),
                        actionButton("loginButton", "Login"),
                        hr()
                      ),
                      
                      uiOutput("selectAss")
                    ),
                    
                    # BODY -------------------------------------------------------------------
                    dashboardBody(
                      
                      includeCSS("www/myCSS.css"),
                      
                      # Show content only when logged in
                      conditionalPanel(
                        condition = "output.isLoggedIn == true",
                        fluidRow(
                          h1("Data export"),
                          br(),
                          downloadButton("downloadData", "Download Data"),      
                          br(),
                          br(),
                          uiOutput("downloadStatus"),
                          br(),
                          tags$p("Downloaded file will be a CSV with semicolon used to separate colums."),
                          tags$p("The CSV is generated on-demand and processed in batches to handle large datasets efficiently."),
                          tags$p("Each batch of patient data is written directly to the file as it's processed.")
                        ),
                        fluidRow(
                          br(), 
                          br()
                        )
                      ),
                      
                      # Show login message when not logged in
                      conditionalPanel(
                        condition = "output.isLoggedIn != true",
                        fluidRow(
                          box(
                            width = 12,
                            status = "warning",
                            h3("Not logged in"),
                            p("Please log in using the form in the sidebar or via the main MHIRA application.")
                          )
                        )
                      )
                    )
)

## SERVER ## ----------------------------------------------------------------- 

server = function(input, output, session) {
  
  # OBSERVE INACTIVITY AND CLOSE APP ------------------------------------------  
  observeEvent(input$timeOut, {
    print(paste0("Session was closed at", " ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session was closed after", input$timeOut),
      footer = NULL
    ))
    session$close()
  })
  
  # REACTIVE VALUES FOR AUTH STATE --------------------------------------------
  # Create a reactive value to track login state
  auth <- reactiveValues(
    token = NULL,
    isLoggedIn = FALSE,
    loginMethod = NULL
  )
  
  # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT ----------------
  observe({ 
    req(input$accessToken)
    token_data <- tryCatch({
      parsed <- fromJSON(input$accessToken)
      if(!is.null(parsed$accessToken)) {
        parsed$accessToken
      } else {
        NULL
      }
    }, error = function(e) {
      # Handle JSON parsing error
      print("Error parsing token from localStorage")
      print(e)
      NULL
    })
    
    if(!is.null(token_data)) {
      print("Retrieved token from browser localStorage")
      auth$token <- token_data
      auth$isLoggedIn <- TRUE
      auth$loginMethod <- "localStorage"
      
      # Store in session for backward compatibility
      session$userData <- token_data
    }
  }) %>% bindEvent(input$accessToken)
  
  # MANUAL LOGIN HANDLING ----------------------------------------------------
  observeEvent(input$loginButton, {
    req(input$username, input$password)
    
    # Show a notification that login is being attempted
    id <- showNotification("Logging in...", type = "message", duration = NULL)
    
    # Try to get token
    token_result <- tryCatch({
      getToken(Username = input$username, Password = input$password, url = url)
    }, error = function(e) {
      print(paste("Login error:", e$message))
      NULL
    })
    
    # Remove the notification
    removeNotification(id)
    
    if(!is.null(token_result)) {
      # Login successful
      auth$token <- token_result
      auth$isLoggedIn <- TRUE
      auth$loginMethod <- "manual"
      
      # Store in session for backward compatibility
      session$userData <- token_result
      
      showNotification("Login successful", type = "message")
    } else {
      # Login failed
      showNotification("Login failed. Check credentials and try again.", 
                       type = "error", 
                       duration = 5)
    }
  })
  
  # UI OUTPUTS FOR LOGIN STATE -----------------------------------------------
  output$isLoggedIn <- reactive({
    auth$isLoggedIn
  })
  outputOptions(output, "isLoggedIn", suspendWhenHidden = FALSE)
  
  output$loginStatus <- renderUI({
    if(auth$isLoggedIn) {
      div(
        style = "padding: 10px;",
        div(
          style = "color: green; font-weight: bold;",
          icon("check-circle"), "Logged In"
        ),
        p(paste0("Method: ", auth$loginMethod)),
        actionButton("logoutButton", "Logout", class = "btn-sm btn-warning")
      )
    } else {
      div(
        style = "padding: 10px;",
        div(
          style = "color: red;",
          icon("exclamation-triangle"), "Not Logged In"
        )
      )
    }
  })
  
  # LOGOUT FUNCTIONALITY -----------------------------------------------------
  observeEvent(input$logoutButton, {
    auth$token <- NULL
    auth$isLoggedIn <- FALSE
    auth$loginMethod <- NULL
    session$userData <- NULL
    
    # Clear patient data
    patientIds(NULL)
    response(NULL)
    data(NULL)
    
    showNotification("Logged out successfully", type = "message")
  })
  
  # GET PATIENT IDS FROM MHIRA -----------------------------------------------
  patientIds = reactiveVal()
  
  observe({
    req(auth$isLoggedIn)
    req(auth$token)
    print("Getting patient IDs from MHIRA")
    
    patients = getPatientIds(token = auth$token, url = url) 
    
    if (length(patients) > 0) {
      # Store the full patients data frame
      patientIds(patients)
    } else {
      showNotification(
        "No patients found",
        type = "error",
        duration = 20)
    }
  }) %>% bindEvent(auth$token)
  
  # STREAMLINED DATA PROCESSING AND DOWNLOAD ---------------------------------------
  # This approach avoids storing all data in memory by writing directly to file
  
  # Create a reactive value to track data availability and processing status
  dataStatus <- reactiveValues(
    isProcessing = FALSE,
    hasData = FALSE,
    patientCount = 0,
    batchesProcessed = 0,
    totalBatches = 0,
    lastProcessedTime = NULL
  )
  
  # Setup streamlined download handler that processes data on-demand
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      req(auth$isLoggedIn, auth$token)
      
      # Set processing flag
      dataStatus$isProcessing <- TRUE
      
      # Extract patient IDs
      all_patient_ids <- unique(patientIds()$id)
      dataStatus$patientCount <- length(all_patient_ids)
      
      # Split patient IDs into smaller batches
      batch_size <- 100
      patient_id_batches <- split(all_patient_ids, ceiling(seq_along(all_patient_ids)/batch_size))
      dataStatus$totalBatches <- length(patient_id_batches)
      dataStatus$batchesProcessed <- 0
      
      # Process each batch and write directly to file
      withProgress(message = 'Generating CSV file', value = 0, {
        
        # Flag to track if any data was written
        data_written <- FALSE
        
        for (i in seq_along(patient_id_batches)) {
          batch <- patient_id_batches[[i]]
          incProgress(1/dataStatus$totalBatches, 
                      detail = paste("Processing batch", i, "of", dataStatus$totalBatches))
          
          print(paste("Processing batch", i, "of", dataStatus$totalBatches, 
                      "with", length(batch), "patients"))
          
          # Fetch batch data
          batch_response <- tryCatch({
            getMultiplePatientReports(token = auth$token, patientIds = batch, url = url)
          }, warning = function(w) {
            if (grepl("Session expired! Please login.", w$message)) {
              showNotification("Session has expired! Please login again.", 
                               type = "error", duration = 20)
              auth$isLoggedIn <- FALSE
              auth$token <- NULL
              auth$loginMethod <- NULL
              return(NULL)
            }
            warning(w)  # Re-raise other warnings
            return(NULL)
          }, error = function(e) {
            print(paste("Error fetching batch", i, ":", e$message))
            return(NULL)
          })
          
          # Process batch if valid
          if (!is.null(batch_response) && 
              !is.null(batch_response$data$generateMultiplePatientReports) && 
              length(batch_response$data$generateMultiplePatientReports) > 0) {
            
            # Simplify the response data
            batch_df <- simplifyMultPatRep(response = batch_response)
            
            if (!is.null(batch_df) && nrow(batch_df) > 0) {
              # Remove complex columns
              batch_df <- batch_df %>%
                select(-any_of(c("questionnaireScripts", "choices", "multipleChoiceValue")))
              
              # Write to CSV - append if not the first batch
              tryCatch({
                if (i == 1) {
                  # First batch - write with header
                  fwrite(batch_df, file, sep = ";")
                  data_written <- TRUE
                } else if (data_written) {
                  # Subsequent batches - append without header
                  fwrite(batch_df, file, sep = ";", append = TRUE, col.names = FALSE)
                } else {
                  # If previous batches failed, write with header
                  fwrite(batch_df, file, sep = ";")
                  data_written <- TRUE
                }
                
                dataStatus$batchesProcessed <- i
                dataStatus$hasData <- TRUE
                
              }, error = function(e) {
                print(paste("Error writing batch", i, "to CSV:", e$message))
              })
            }
          }
          
          # Add a small delay between batches
          Sys.sleep(0.5)
        }
        
        # If no data was written successfully, create an error message HTML file
        if (!data_written) {
          writeLines(
            paste0("<html><body><h2>No data available</h2><p>",
                   "No patient data could be processed. Please check your access permissions or try again later.</p></body></html>"),
            file
          )
        }
      })
      
      # Update status
      dataStatus$isProcessing <- FALSE
      dataStatus$lastProcessedTime <- Sys.time()
    }
  )
  
  # Create a simple UI element to show processing status
  output$downloadStatus <- renderUI({
    if (dataStatus$isProcessing) {
      div(
        style = "margin-top: 15px;",
        p(paste("Processing data for", dataStatus$patientCount, "patients in", 
                dataStatus$totalBatches, "batches.")),
        p(paste("Batches processed:", dataStatus$batchesProcessed, "of", dataStatus$totalBatches))
      )
    } else if (!is.null(dataStatus$lastProcessedTime)) {
      div(
        style = "margin-top: 15px;",
        p(paste("Last processed:", format(dataStatus$lastProcessedTime, "%Y-%m-%d %H:%M:%S")))
      )
    }
  })
  
}

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)