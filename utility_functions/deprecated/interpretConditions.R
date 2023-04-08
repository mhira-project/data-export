applyCutOffs = function(scales, interpret){
  
 cut =  read.table(text = interpret, sep = ";", header = 1)

 conditionResultTable = data.frame()
 
 for (i in unique(scales$questionnaireId)){
   
  data = scales %>% 
     filter(questionnaireId == i) %>%
     select(variable, value) %>% 
     pivot_wider(names_from = variable, values_from = value) %>%
     as.data.frame()
  
  for (c in 1:nrow(cut)){
   cut$conditionResult[c] =  eval(parse(text = cut$condition[c]), env = data)
  } 
  
cutOffTable = cut %>% bind_cols(scales %>% 
    filter(questionnaireId == i) %>% 
    slice_head %>% 
    select(assessmentId,
           assessmentName,
           questionnaireId,
           questionnaireShortName,
           questionnaireFullName,
           language,
           assessmentDateTime))

conditionResultTable =cutOffTable %>%
  bind_rows(conditionResultTable)


 }
 
 return(conditionResultTable)

}




