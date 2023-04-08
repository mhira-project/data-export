getMhiraAssessments1 = function(token, patientFilter){
  print("getMhiraAssessments1")

# Packages 
  
# Settings ----
source("settings.R")

# Load functions ----
source("graphql_functions/GQL2.R")
source("graphql_functions/generateAssessmentsQuery.R")

# GraphQL request to get assessments ----
assessments_query = generateAssessmentsQuery(patientFilter)
res_assessments = GQL2(assessments_query, .token = token, .url = url)
res_assessments <- fromJSON(res_assessments)
assessments = res_assessments$data$assessments$edges$node 

# assessments = assessments[assessments$status == "COMPLETED",] # currently not working because field is exported from postgres
print("getMhiraAssessments1 exit")
return(assessments)

}
