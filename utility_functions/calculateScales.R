
calculateScales = function(simplifiedData, questionnaireScripts) {
 print("calculateScales")
  
  qS = questionnaireScripts %>%
    filter(name == "scales_table" | name == "scales_function")
  
QuestionnairesHaveScript =  unique(simplifiedData$questionnaireVersionId) %in% unique(qS$questionnaireId) 
noMatchingScripts =   (!QuestionnairesHaveScript) %>% all()
 

 if(is_empty(qS) | noMatchingScripts){
    showNotification(
      "No scale_script or scales_table found for the collected data.
      You need to add these evaluation routines to your questionnaires",
      type = "error",
      duration = 20)
    session$close()}
  

d = data.frame() # Empty data frame to collect the result

  
 for (i in unique(simplifiedData$questionnaireId)){
 
   
   #Select a questionnaire
   
   df = simplifiedData[simplifiedData$questionnaireId == i,]
   
   # Search for scripts for this questionnaire
   
   dfScr = qS %>%
     filter(questionnaireId == df$questionnaireVersionId %>% unique)  
   
   if(nrow(dfScr) == 0){next}
   if(nrow(dfScr) != 1){
     showNotification(
       paste("More than one scale evaluation routine (scales_table or scales_script) for",
             df$questionnaireShortName,
             df$language,
             ". Only one is allowed.",
             sep = " "), 
       type = "error",
       duration = 20)
     session$close()}
   
   # Prepare questionnaire data for application of script
  
   df$textValue = as.numeric(df$textValue)
   dfItems = t(df[c("textValue")])
   colnames(dfItems) = df$variable  
   dfItems = as.data.frame(dfItems)
   
   # Apply script to questionnaire data and store response in "scales"

  scales =  data.frame(scale=character(0),value=integer(0))
    
  if(dfScr$name == "scales_table"){
      scr = fread(dfScr$scriptText)   
      
     for (s in 1:nrow(scr)){
      
       ex =  lapply(X = all.vars(parse(text = scr$Formula[s])),
                    FUN = exists,
                    dfItems) %>%
                    unlist
       
       if(!all(ex)){
         showNotification(
           ui = paste("In the questionnaire with the version",
                      simplifiedData$questionnaireVersionId[simplifiedData$questionnaireId == i] %>%
                      unique(),
                      ": some of the variables used in scales_table do not exist in the questionnaire. 
                      The corresponding scales are omitted in the report."),
            duration = 20,
            closeButton = T
            )
           next
                  }
       
       scales[s,"value"] <- eval(parse(text = scr$Formula[s]), envir = dfItems)
      
       
       scales[s, "scale"] <- scr$ScaleName[s]
       }
  
       scales = bind_cols(scales, scr %>% select(scaleMin, scaleMax, mean, sd, plotGroup))
      
  }
  
  if(dfScr$name == "scales_function"){
 
     dfScr$scriptText =  gsub("\\r", "", dfScr$scriptText)
     eval(parse(text = dfScr$scriptText))   
     scales = scales_function(dfItems)
     
     }
  
  
  
if(!all(c("scale" ,"value", "scaleMin", "scaleMax", "plotGroup") %in% colnames(scales))){
  showNotification(
    paste("The scales_table or scales_function did not produce the required colums for:",
          df$questionnaireShortName,
          df$language,
          sep = " "), 
    type = "warning",
    duration = 15)
    next}
  

  x = df %>% select(assessmentId, assessmentName, questionnaireId,questionnaireVersionId,
                questionnaireShortName, questionnaireFullName,
                language, assessmentDateTime =  updatedAt) %>%
    drop_na(assessmentDateTime) %>%
    slice_tail() %>%
    bind_cols(scales)
  
 d =  bind_rows(d, x)  
   
  
 } 
  
d$assessmentDate = as.Date(d$assessmentDateTime)
d$assessmentName = factor(d$assessmentName, levels = d[order(d$assessmentDateTime, decreasing = F),c("assessmentName")] %>% unique)


  
  return(d)
  
  
}
