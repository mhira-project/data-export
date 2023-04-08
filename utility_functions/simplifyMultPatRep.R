# Packages ----
library(tidyverse)

# Function ----

simplifyMultPatRep = function(response){
  
  if(is_empty(response)){stop(paste("No data was supplied. Problem with graphql response:", response$errors$message))}
  
  data = response$data$generateMultiplePatientReports
  
  
  df = data %>%
    unnest(cols = "answeredQuestionnaires") %>%
    add_column(. , questionnaireId = seq_len(nrow(.))) %>%
    unnest("questions") %>%
    unnest("patient") %>% 
    unnest("answer")
  
  assessments = df$assessments %>% list_rbind() %>% unnest("assessmentType")  %>% unique.data.frame() 

  df = df %>% right_join(assessments, by = "assessmentId") %>% select(-assessments)

  colnames(df)[colnames(df) == "name.x"] <- "questionnaireShortName"
  colnames(df)[colnames(df) == "name.y"] <- "assessmentName"
  colnames(df)[colnames(df) == "status"] <- "assessmentStatus"
  colnames(df)[colnames(df) == "_id"] <- "questionnaireVersionId"
  colnames(df)[colnames(df) == "deliveryDate"] <- "assessmentDeliveryDate"
  colnames(df)[colnames(df) == "submissionDate"] <- "assessmentSubmissionDate"
  colnames(df)[colnames(df) == "emailStatus"] <- "assessmentEmailStatus"
  


 # Order columns
 df = df %>% 
   relocate(assessmentName, assessmentStatus, assessmentDeliveryDate,
            assessmentSubmissionDate, assessmentEmailStatus,
            questionnaireId,  .after = assessmentId)  
 

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
          updatedAt = lubridate::as_datetime(updatedAt),
          assessmentSubmissionDate = lubridate::as_datetime(assessmentSubmissionDate),
          assessmentDeliveryDate = lubridate::as_datetime(assessmentDeliveryDate)
          ) 

  
 df = df %>% mutate(questionnaireShortName = coalesce(questionnaireShortName,questionnaireFullName) # in case short name is missing
                    )
  # Return simplfied dataframe
 
 return(df)

}
