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
     
     uiOutput("selectAss")
   
     ),
    
    # BODY -------------------------------------------------------------------
    dashboardBody(
       
      includeCSS("www/myCSS.css"),
            fluidRow(
        h1("Data export"),
        br(),
        downloadButton("downloadData", "Download Data"),      
          br(),
          br(),
        tags$p("Downloaded file will be a CSV with semicolon used to separate colums."),
        tags$p("Loading the data can take a moment... In case data is not ready, a html file will be downloaded instead of the CSV. In this case, try again after a moment.")
        ), 
   
      fluidRow(


          
           
            br(), 
            br()
            
        )
        
      
      )

    
  # CLOSE USER INTERFACE UI ---------------------------------------------------
    
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
  
  # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT ----------------
  observe({ 
    print("writing token to session object")
    session$userData = fromJSON(input$accessToken)$accessToken
  }) %>% bindEvent(input$accessToken)
  
  
  # GET PATIENT IDS FROM MHIRA -----------------------------------------------
  patientIds = reactiveVal()
  
  observe({
    req(!is_empty(session$userData))
    print("getting patient IDs from MHIRA")
    
    patients = getPatientIds(token = session$userData, url = url) 
    
    if (length(patients) > 0) {
      # Store the full patients data frame
      patientIds(patients)
    } else {
      showNotification(
        "No patients found",
        type = "error",
        duration = 20)
      session$close()      
    }
  }) %>% bindEvent(input$accessToken)
  
  # GET PATIENT REPORT DATA ---------------------------------------------------
  response = reactiveVal()
  
  observe({
    req(!is_empty(patientIds()))
    req(!is_empty(session$userData))
    print("Get patient report via GraphQL in batches")
    
    # Extract just the ID column from the patients dataframe
    all_patient_ids <- unique(patientIds()$id)
    
    # Split patient IDs into smaller batches
    batch_size <- 100
    patient_id_batches <- split(all_patient_ids, ceiling(seq_along(all_patient_ids)/batch_size))
    
    # Initialize an empty list to store the results
    all_patient_data <- list()
    
    for (batch in patient_id_batches) {
      print(paste("Fetching data for batch of", length(batch), "patients"))
      
      batch_response <- tryCatch({
        getMultiplePatientReports(token = session$userData, patientIds = batch, url = url)
      }, warning = function(w) {
        if (grepl("Session expired! Please login.", w$message)) {
          showNotification("Session has expired! Please login again.", 
                           type = "error", duration = 20)
          session$close()
          return(NULL)
        }
        warning(w)  # Re-raise other warnings
      }, error = function(e) {
        showNotification("An error occurred while fetching patient data.", 
                         type = "error", duration = 20)
        print(e)  # Log the error for debugging
        return(NULL)
      })
      
      if (!is.null(batch_response) && 
          !is.null(batch_response$data$generateMultiplePatientReports) && 
          length(batch_response$data$generateMultiplePatientReports) > 0) {
        
        response_df <- simplifyMultPatRep(response = batch_response)
        
        if (!is.null(response_df) && nrow(response_df) > 0) {
          all_patient_data[[length(all_patient_data) + 1]] <- response_df
        }
      }
      
      # Add a small delay between batches to avoid overwhelming the server
      Sys.sleep(0.5)
    }
    
    # Combine all the batch results into a single dataframe
    if (length(all_patient_data) > 0) {
      combined_df <- bind_rows(all_patient_data)
      response(combined_df)
    }
    
  }) %>% bindEvent(patientIds())
  
  # PROCESS AND PREPARE DATA FOR DOWNLOAD -------------------------------------
  data = reactiveVal()
  
  observe({
    req(!is_empty(response()))
    
    current_data <- response()
    
    # Verify data validity
    if (is.null(current_data) || nrow(current_data) < 1) {
      showNotification(
        "Problem with data. Does the user have access to patient data?",
        type = "error",
        duration = 20)
      session$close()
      return()
    }
    
    # Store the processed data
    data(current_data)
    
    # Render the data table
    output$data <- renderDT({
      data()
    })
    
    # Setup download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
      },
      content = function(file) {
        # Prepare data for export by removing complex columns
        export_data <- data() %>%
          select(-any_of(c("questionnaireScripts", "choices", "multipleChoiceValue")))
        
        # Write to CSV with error handling
        tryCatch({
          fwrite(export_data, file, sep = ";")
        }, error = function(e) {
          # If CSV writing fails, create an error message HTML file
          writeLines(
            paste0("<html><body><h2>Error generating CSV</h2><p>",
                   "Please try again in a moment. If the problem persists, ",
                   "contact your system administrator.</p></body></html>"),
            file
          )
        })
      }
    )
    
  }) %>% bindEvent(response())
  
}

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)