getPatientIds = function(token, url){
  print("getPatients")
  
  # Load functions ----
  source("graphql_functions/GQL2.R")
  source("graphql_functions/generatePatientIdQuery.R")
  
  all_patients <- data.frame()
  last_cursor <- NULL
  hasNextPage <- TRUE
  iteration <- 1
  
  while (hasNextPage) {
    print(paste("Iteration: ", iteration))
    
    # Prepare the paging string
    paging_string <- if (!is.null(last_cursor)) {
      paste0('after: "', last_cursor, '"')
    } else {
      ""
    }
    
    # Generate the query
    patient_query = generatePatientIdQuery(paging_string)
    
    # Make the GraphQL request
    res_pat = GQL2(patient_query, .token = token, .url = url)
    res_pat <- fromJSON(res_pat)
    
    # Append to all_patients
    new_patients <- as_tibble(res_pat$data$patients$edges$node)
    all_patients <- rbind(all_patients, new_patients)
    
    # Check if there's a next page
    hasNextPage <- res_pat$data$patients$pageInfo$hasNextPage
    print(paste("Has next page: ", hasNextPage))
    
    # Get the last cursor if it exists
    if (hasNextPage) {
      last_cursor <- tail(res_pat$data$patients$edges, 1)$cursor
      print(paste("Last cursor: ", last_cursor))
    }
    
    iteration <- iteration + 1
  }
  
  return(all_patients)
}
