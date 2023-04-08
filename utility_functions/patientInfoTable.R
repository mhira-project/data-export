patientInfoTable = function(response, selectPatientInfo, render = T){
  
  if(is_empty(response$data)){
    stop(paste("No data was supplied. Problem with graphql response:",
     response$errors$message))}
  
  df = response$data$generatePatientReport$patient %>%
       map(.f =  ~ifelse(is.null(.x), NA, .x)) %>% 
       as_tibble()
  
 df$initials = paste(df$firstName %>% substr(1,1) %>% toupper(),
                     df$lastName %>% substr(1,1) %>% toupper(),
                     sep = "")

 
 df$age =  (difftime(Sys.Date(), as.Date(df$birthDate), units = "weeks")/52.25) %>% 
                as.numeric() %>%
                round(1)
                          
 df = df %>% select(all_of(selectPatientInfo)) 
 
 
 dfRendered = df %>% 
   renderDT(options = list(dom = 't'), rownames= FALSE) 
   
if(render == TRUE){return(dfRendered)} else {return(df)}
  
}
