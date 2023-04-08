getPatients = function(token){
  print("getPatients")



# Packages

# Load functions ----
source("graphql_functions/GQL2.R")
source("graphql_functions/generatePatientQuery.R")


#-- Settings ----
source("settings.R")

# GraphQL request to get patient list ----
patient_query = generatePatientQuery()
res_pat = GQL2(patient_query, .token = token, .url = url)
res_pat <- fromJSON(res_pat)
pat = as_tibble(res_pat$data$patients$edges$node)


return(pat)

}
