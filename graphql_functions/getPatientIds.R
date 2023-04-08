getPatientIds = function(token, url){
  print("getPatients")

# Packages

# Load functions ----
source("graphql_functions/GQL2.R")
source("graphql_functions/generatePatientIdQuery.R")


# GraphQL request to get patient list ----
patient_query = generatePatientIdQuery()
res_pat = GQL2(patient_query, .token = token, .url = url)
res_pat <- fromJSON(res_pat)
pat = as_tibble(res_pat$data$patients$edges$node)


return(pat)

}
