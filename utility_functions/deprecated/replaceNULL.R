replaceNULL <- function(x) {
  
  modify_if(x, is.list, replaceNULL, .else = ~ifelse(is.null(.), NA, .))
  
}