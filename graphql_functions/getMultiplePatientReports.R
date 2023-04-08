# Load packages ----
library(tidyverse)
library(jsonlite)
#library(tidyjson)
library(data.table)


getMultiplePatientReports = function(token, patientIds, url){
  print("getMultiplePatientsReports")
  
  # Load functions ----
  source("graphql_functions/GQL2.R")
  source("graphql_functions/generateMultiplePatientReportsQuery.R")
  
  

  # GraphQL request to get patient list ----
  query = generateMultiplePatientReportsQuery(patientIds)
  response = GQL2(query, .token = token, .url = url)  %>%
    fromJSON(simplifyVector = T)
  
  return(response)
  
  }