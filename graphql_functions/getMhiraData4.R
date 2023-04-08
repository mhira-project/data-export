getMhiraData4 = function(token, assessmentIds){
print("getMhiraData4")
  
# Packages
library(jsonlite)
library(tidyverse)
library(data.table)
library(tidyjson)
  
# Load functions ----
source("graphql_functions/GQL.R")
source("graphql_functions/GQL2.R")
source("graphql_functions/generateFullAssessmentQuery.R")
source("utility_functions/replaceNULL.R")

#-- Settings ----
source("settings.R")


#GraphQL request to get Full Assessment (+ answers to questions) ----

answers = data.frame()
choices = data.frame()
Ans = data.frame()


for (a in assessmentIds){

# To do: Check for status completed
fullAssessment_query = generateFullAssessmentQuery(a)
fullAssessmentsAnswer = GQL(fullAssessment_query, .token = token, .url = url)
fullAssessmentsAnswer = fullAssessmentsAnswer[[1]]


fullAssessmentsAnswer = replaceNULL(fullAssessmentsAnswer)

l = fullAssessmentsAnswer$questionnaireAssessment$answers %>% modify_depth(2, function(x){ifelse(is_empty(x), NA, x)})
   
Answers = as.data.frame(rbindlist(l))



if (nrow(Answers) < 1){next} 

Answers = data.frame(Answers,
                          fullAssessmentsAnswer[c(
                          "assessmentId"= "id", "name","patientId",
                          "clinicianId","status",
                          "createdAt", "updatedAt")]) 

colnames(Answers) = c("questionId",
                      "textValue",
                      "multipleChoiceValue",
                      "numberValue",
                      "dateValue",
                      "booleanValue",
                      "assessmentId",
                      "assessmentName",
                      "patientId",
                      "clinicianId",
                      "assessmentStatus",
                      "assessmentCreatedAt",
                      "assessmentUpdatedAt")
                         
# Get list of questions and questionnaires
   
q = fullAssessmentsAnswer$questionnaireAssessment$questionnaires 

questionnaireInfo = data.frame()
questionList = data.frame()
choicesList = data.frame()
qInfoNames <- c("_id", "name", "status", "createdAt", "copyright", "questionnaire")

for  (ql in 1:length(q)){
  qi <- data.frame( matrix(unlist(q[[ql]][qInfoNames], use.names = T), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
  questionnaireInfo = rbind(questionnaireInfo, qi) 
  colnames(qi)<- c("questionnaireId", "questionnaireFullName", "questionnaireStatus", "questionnaireCreatedAt", "questionnaireCopyright",  "questionnaireLanguage", "questionnaireName")
 
  
  z = purrr::flatten(q[[ql]]$questionGroups)
  
for (w in 1: length(z$questions)){
  qw = data.frame( qi, rbindlist(map(z, w))) 
  questionList = rbind(questionList, qw)
  
 
  
}

   
  
  rm(qi)  
  
}

colnames(questionList)[colnames(questionList) == "X_id"] = "questionId" 



# Choices
questionList$choices = replaceNULL(questionList$choices)
choices = data.frame()
for (questID in unique(questionList$questionId)){
  
  u = questionList$choices[questionList$questionId == questID ] 
  if(all(is.na(unique(unlist(u))))) {next}
  u = tibble(questionId = questID,rbindlist(u))
  choices = rbind(choices, u)
}


questions = questionList %>% select(-choices) %>% unique

Answers$multipleChoiceValue = replaceNULL( Answers$multipleChoiceValue)
Answers$multipleChoiceValue = unlist(Answers$multipleChoiceValue)


ans =  Answers %>% group_by(questionId) %>%  summarise(multipleChoiceValue = list( multipleChoiceValue)) 
ans = Answers %>% select(-multipleChoiceValue ) %>% unique %>% full_join(ans)
answersList = questions %>% right_join(ans, by = "questionId")

 
choices = rbind(choices, choicesList)
answers = rbind(answers, answersList)


}

if (nrow(answers) > 0){
Ans = answers %>% left_join(choices %>% select(questionId, name, value = label), by = c('questionId' = 'questionId', 'textValue' = 'name' ))

for (r in which(is.na(Ans$value))){
  Ans$value[r] <- Ans$textValue[r]
  if(is.na(Ans$value[r])) Ans$value[r] <- {Ans$numberValue[r]}
  if(is.na(Ans$value[r])) Ans$value[r] <- {Ans$dateValue[r]}
  if(is.na(Ans$value[r])) Ans$value[r] <- {Ans$booleanValue[r]}
  if(is.na(Ans$value[r])) Ans$value[r] <- paste(choices$label[choices$questionId == Ans$questionId[r] & choices$name %in%  unlist(Ans$multipleChoiceValue[r])], collapse = ";")
  
}} 



 print("getMHIRAData_Exit")
 return(Ans)


}







