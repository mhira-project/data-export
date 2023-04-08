# Packages ----
library(tidyverse)

# Function ----

simplifyPatRep = function(response){
  
  if(is_empty(response$data)){stop(paste("No data was supplied. Problem with graphql response:", response$errors$message))}
  data = as_tibble(response$data)
  
  df = data$
    generatePatientReport$
    answeredQuestionnaires %>%
    add_column(. , questionnaireId = seq_len(nrow(.))) %>%
    unnest(cols = "questions") %>%
    unnest(cols = "answer") %>%
    left_join(unnest(data$generatePatientReport$assessments, cols = "assessmentType"), by = "assessmentId") 
  
  if(!"createdAt" %in% colnames(df)){return(NULL)}
  
  colnames(df)[colnames(df) == "name.x"] <- "questionnaireShortName"
  colnames(df)[colnames(df) == "name.y"] <- "assessmentName"
  colnames(df)[colnames(df) == "status"] <- "assessmentStatus"
  colnames(df)[colnames(df) == "_id"] <- "questionnaireVersionId"

 # add questionnaireStatus
 df$required[is.na(df$required)] <- FALSE
 df = df %>% group_by(questionnaireId) %>% mutate(
   questionnaireCompleted = all(!is.na(createdAt[required]))) %>% ungroup() 
 
 # Remove uncompleted questionnaires
 df = df %>% filter(questionnaireCompleted) %>% select(-questionnaireCompleted)
 
 # Order columns
 df = df %>% 
   relocate(assessmentName, assessmentStatus, questionnaireId,  .after = assessmentId)  
 
 # All answer types to one column --- nope
 
 # Add choice labels
 
 df$selectOneChoice = NA
 df$selectMultipleChoices = NA
   
  for (i in 1:nrow(df)){ 
   c = df$choices[i] %>% as.data.frame()
   if(is_empty(c)){next}
  
   if(df$type[i] == "select_one" & !is.na(df$type[i])){
     df$selectOneChoice[i] = c$label[c$name == df$textValue[i]]
      }
   
   if(df$type[i] == "select_multiple" & !is.na(df$type[i])){
     df$selectMultipleChoices[i] <- c$label[c$name == df$multipleChoiceValue[i] %>% unlist %>% as.numeric] %>% list 
      }
   } 
  
 df = df %>% 
   relocate(selectOneChoice, selectMultipleChoices,  .after = choices)  %>% 
   mutate(createdAt = lubridate::as_datetime(createdAt),
          updatedAt = lubridate::as_datetime(updatedAt)) 

  
 df = df %>% mutate(questionnaireShortName = coalesce(questionnaireShortName,questionnaireFullName) # in case short name is missing
                    )
  # Return simplfied dataframe
 
 return(df)

}
