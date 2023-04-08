# Load packages ----
library(tidyverse)
library(jsonlite)
#library(tidyjson)
library(data.table)


getPatientReport = function(token, patientId, url){
  print("getPatientsReport")
  
  # Load functions ----
  source("graphql_functions/GQL2.R")
  source("graphql_functions/generatePatientReportQuery.R")
  source("graphql_functions/getUserProfile.R")
  

  # GraphQL request to get patient list ----
  query = generatePatientReportQuery(patientId)
  response = GQL2(query, .token = token, .url = url) %>%
    fromJSON()
  
  return(response)
  
  }