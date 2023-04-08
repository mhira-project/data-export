printItemTable = function(data, questVersionId){
  
  #data$createdAt = sub(" ", "<br/>", as.character(data$createdAt))
  #data$updatedAt = sub(" ", "<br/>", as.character(data$updatedAt))
  
   df = data %>% 
    filter(questionnaireVersionId ==  questVersionId) %>%
    filter(!is.na(createdAt)) %>%
    mutate(assessmentName = factor(assessmentName),
           selectOneChoice = factor(selectOneChoice),
           assessmentId = assessmentId %>% factor() %>% as.numeric() %>% factor()) %>%
    select(Assessment = assessmentName,
           Answer_time =  createdAt,
           Question =  label,
           Selected_choice =  selectOneChoice,
           Value =  textValue,
           Context = questionGrouplabel, 
           ID = assessmentId) %>%
    arrange(Answer_time) 
  
  
  
  dfRendered = df %>% 
    datatable(options = list(order = list(list(2, 'asc')), pageLength = 100),
              escape = FALSE, filter = "top") %>% 
    #  formatStyle(valueColumns = transMatrix["warning", lang], # https://rstudio.github.io/DT/010-style.html
    #              columns = c(transMatrix["warning", lang]), 
    #              backgroundColor = styleEqual(c(TRUE, FALSE), c('red', 'green')),
    #              color = styleEqual(c(TRUE, FALSE), c('red', 'green'))
    #        ) %>% 
    renderDT() 
  
  
  return(dfRendered)
  
}