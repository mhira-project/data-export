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
source("utility_functions/split_patient_ids.R")

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
        paste("Session was closed afer",
              input$timeOut
        ),
        footer = NULL
      ))
      session$close()
    })
    
  # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT ----------------
    observe({ 
      print("writing token to session object")
      session$userData  = fromJSON(input$accessToken)$accessToken
    }) %>%  bindEvent(input$accessToken)
  
    
  # GET PATIENT IDS FROM MHIRA
    
    patientIds =  reactiveVal()

    

    observe({
      req(!is_empty(session$userData))
      print("getting patient IDs from MHIRA")
      
      Patients = getPatientIds(token = session$userData, url = url) 

      if (length(Patients) > 0){
          patientIds(Patients$id)
      } else {
           showNotification(
             "No patients found",
             type = "error",
             duration = 20)
           session$close()      
      }
    }) %>%  bindEvent(input$accessToken)
    
    
  
     
  # GET PATIENT REPORT DATA ---------------------------------------------------
  
    response = reactiveVal() # the imported data as dataframe


observe({
    req(!is_empty(patientIds()))
    req(!is_empty(session$userData))
    print("Get patient report via GraphQL in batches")
    
    all_patient_ids <- patientIds() %>% pull(id) %>% unique()
    
    # Split patient IDs into smaller batches, e.g., 100 IDs per batch
    batch_size <- 100
    patient_id_batches <- split_patient_ids(patientIds, batch_size)
    
    # Initialize an empty list to store the results
    all_patient_data <- list()
    
    for (batch in patient_id_batches) {
      print(paste("Fetching data for batch of", length(batch), "patients"))
      print(batch)
      
      batch_response <- tryCatch({
        getMultiplePatientReports(token = token, patientIds = batch, url = url)
      }, warning = function(w) {
        if (grepl("Session expired! Please login.", w$message)) {
           showNotification("Session has expired! Please login again.", type = "error", duration = 20)
            session$close()
          return(NULL)  # Return NULL to indicate failure
        }
      }, error = function(e) {
          showNotification("An error occurred while fetching patient IDs.", type = "error", duration = 20)
          session$close()
        return(NULL)  # Return NULL to indicate failure
      })
      
      if (is.null(batch_response$data$generateMultiplePatientReports) || length(batch_response$data$generateMultiplePatientReports) == 0) {next}
      
      print(batch_response)
      
      if (exists("batch_response") && !is.null(batch_response)) {
        response_df <- simplifyMultPatRep(response = batch_response)
      }
      
      
      
      if (exists("response_df") && !is.null(response_df)) {
        print(response_df)
        all_patient_data <- append(all_patient_data, list(response_df))
      }
    }
    
    # Combine all the batch results into a single dataframe
    combined_df <- bind_rows(all_patient_data)
    
    # Update the reactive value with the combined dataframe
    response(combined_df)
    
  }) %>% bindEvent(patientIds())

      
  
  
  # CALCULATE SCALES AND APPLY CUTOFFS -----------------------------------------  
    
    data = reactiveVal() # the imported data as dataframe
    scales = reactiveVal() # calculated scales
    cutoffs = reactiveVal() # only cutoffs without data

    observe({
      req(!is_empty(response()))

      response = response()


    data = response

      # Terminate session if no completed data
      dataNotOkay = FALSE
      if(is_empty(data)){
        dataNotOkay = TRUE
      } else {
       if(data %>% nrow < 1){dataNotOkay = TRUE}
      }

      if(dataNotOkay){
        showNotification(
          "Problem with data. Does the user have access to patient data?",
          type = "error",
          duration = 20)
        session$close()
        }

      # Extract data from scripts

      # questionnaireScripts = response$data$generatePatientReport$questionnaireScripts
      # 
      # cutoffs = extract_cutoffs(questionnaireScripts = questionnaireScripts)
      # cutoffs = groupCutoffs(cutoffs = cutoffs)
      # 
      # scales = calculateScales(
      #             simplifiedData = data,
      #             questionnaireScripts =  questionnaireScripts)
      # 
      # 
      # scales = applyCutOffs(scales = scales, cutoffs = cutoffs)


      data(data)
      output$data = data %>% renderDT()
      dataForDownload = data %>% select(-questionnaireScripts, -choices, -multipleChoiceValue) %>% as.data.frame
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = "")
        },
        content = function(file) {
          fwrite(dataForDownload, file, sep = ";")
        }
      )
      
      
      # scales(scales)
      # cutoffs(cutoffs)
      print("scales have been calculated and cutoffs applied")

            }) %>%  bindEvent(response())
    
  
  }

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)