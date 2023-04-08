# Load packages ----
library(tidyverse)
library(jsonlite)
library(data.table)


getUserProfile = function(token){
  print("getUserProfile")
  
  # Load functions ----
  source("graphql_functions/GQL2.R")
  source("graphql_functions/generateUserProfileQuery.R")
  
  #-- Settings ----
  source("settings.R")
  
  # GraphQL request to get patient list ----
  query = generateUserProfileQuery()
  response = GQL2(query, .token = token, .url = url) %>%
    fromJSON()
  result = response$data %>% flatten 
  
  return(result)
}
