extract_cutoffs = function(questionnaireScripts){
  print("extract_cutoffs")
  
  
  cutoffColnames = c('scale', 'low_cut', 'high_cut', 'level', 'warning', 'text_order', 'interpretation', 'recommendation')
  
  qS = questionnaireScripts %>%
  filter(name == "cutoffs")
  
  if(nrow(qS) == 0){
   cutoffs =   as_tibble(
      matrix(nrow = 0, ncol = length(cutoffColnames) + 1),
      .name_repair = ~ c('questionnaireVersionId',cutoffColnames))
    
    return(cutoffs)
  }
  
 
  cutoffs = data.frame()

  for (r in 1:nrow(qS)){
   
     cutoffs = data.frame(questionnaireVersionId = qS$questionnaireId[r], fread(qS$scriptText[r])) %>%
      bind_rows(cutoffs) 
    
  }
 
requiredColumsCutoffs =  all(cutoffColnames %in% colnames(cutoffs))

if(!requiredColumsCutoffs){
  showNotification(
    "A cutoff file used in at least one of the questionnaires is missing one or more of the required columns",
    type = "error",
    duration = 20)}


cutoffs = cutoffs %>% select(c("questionnaireVersionId", all_of(cutoffColnames))) # if the cutoff files change, this needs to be adapted



 return(cutoffs)

}