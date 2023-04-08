
#tree <- lapply(structure(list(a=list(a1=1,a2=2) , b="b"), stopened = TRUE) , function(x) structure(x, stopened = TRUE))


makeTreeStructure <- function(scales){
  
  f = function(x){
   
    #val$assessmentName = factor(gsub("[[:space:]]", "_", val$assessmentName))
    val = scales %>% select(assessmentName, variable)
    # nestedlist <- lapply(split(val, val$assessmentName, drop = TRUE),
    #                      function(x) split(x, x[['variable']], drop = TRUE))
    
    nestedlist <- lapply(split(val, val$assessmentName, drop = TRUE),
                         function(x) {split(x$variable, x[['variable']], drop = TRUE) })
 
     
     map_depth(nestedlist, 3 ,function(w) {structure(w, stopened = TRUE)})
     map_depth(nestedlist, 1  ,function(w) {structure(w, stselected = TRUE)})

  }
  
  treeStructure = list()
  
  for (q in unique(scales$questionnaireFullName)){
  subTreeStructure =  f(scales %>% select(questionnaireFullName == q))
  treeStructure[[q]] =  subTreeStructure
  
  }
  
  return(treeStructure)
}
