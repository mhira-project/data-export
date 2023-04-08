applyCutOffs = function(scales, cutoffs){
  print("applyCutOffs")
  
 if(cutoffs %>% nrow() < 1) {cutoffs = cutoffs %>% add_row()}
  
  df = scales %>% 
    left_join(cutoffs, by = c("questionnaireVersionId" = "questionnaireVersionId", "scale" = "scale")) 
  
  df1 = df %>% 
    group_by(questionnaireId, scale, assessmentDateTime) %>%          
    mutate(all_cutoffs = paste0("[", level, ": \u2265", low_cut, " & <", high_cut, "]")) %>%
    summarise(all_cutoffs = ifelse(is.na(level), NA, paste(all_cutoffs, collapse = "<br/>"))) %>%
    unique %>%
    ungroup
  
  df2 = df %>%
    left_join(df1)
  
  
  result = df2 %>% 
    group_by(questionnaireId, scale) %>%
    mutate(max_scale = high_cut == max(high_cut))  %>% 
    filter((low_cut <= value & high_cut > value & max_scale == F) | (low_cut <= value & high_cut >= value & max_scale == T) | (is.na(low_cut) & is.na(high_cut))) %>% 
    select(- max_scale) %>% 
    ungroup
  
  
  return(result)
  
}