groupCutoffs = function(cutoffs){
  

  
  g = function(scale, low_cut, high_cut, level){
    
    x = data.frame(scale, low_cut, high_cut, level, group = NA)
    
    
    for (s in 1:length(unique(x$scale))){
      
      if(x$group[x$scale ==  unique(x$scale)[s]] %>% is.na() %>% all()){
        x$group[x$scale ==  unique(x$scale)[s]] <- s} else
        {next}
      
      if(!any(is.na(x$group))){next} 
      
      combs = combn(length(unique(x$scale)), 2) %>% t() %>% as.data.frame()
      testcombs = combs$V2[combs$V1 == s]    
      compareWith = x[x$scale ==  unique(x$scale)[s],c("low_cut", "high_cut",  "level")]  
      
      for (t in testcombs){
        
        if(!x$group[x$scale ==  unique(x$scale)[t]] %>% is.na() %>% all()){next}
        
        if(all.equal(compareWith,
                     x[x$scale ==  unique(x$scale)[t],c("low_cut", "high_cut",  "level")]) %>%
           isTRUE()){
          x$group[x$scale ==  unique(x$scale)[t]] <- s
          
        }
      }
    }
    
    return(x$group) 
    
  }
  
  
  

  
  
 if(cutoffs %>% nrow < 1){
   cutoffs$group <- numeric(0)
   } else {
        cutoffs =  cutoffs %>%
            group_by(questionnaireVersionId) %>%
            mutate(group =   g(scale, low_cut, high_cut, level))
          }
  
  
 return(cutoffs)
}
  
