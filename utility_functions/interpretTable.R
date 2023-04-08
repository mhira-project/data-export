interpretTable = function(scales, transMatrix, lang, showScale = F, render = T){
  

df = scales %>%  
  as.data.frame() %>%
  arrange(desc(assessmentDateTime), assessmentName, text_order) %>%
  select(
    time = assessmentDateTime,
    assessment = assessmentName,
    scales = scale,
    interpretation = interpretation,
    recommendation = recommendation,
    warning = warning) 

df$time = sub(" ", "<br/>", as.character(df$time))

# Summarise across scales

df = df %>% 
        group_by(time) %>%
        mutate(interpretation = paste(interpretation %>% na.omit(), collapse = " "),
               recommendation = paste(recommendation %>% na.omit(), collapse = " "),
               warning = any(warning)) %>%
        select(-scales) %>%
        unique() 

# Replace warning with symbol

df = df %>% mutate(warning = ifelse(warning, as.character(icon("warning", verify_fa = FALSE)), ""))


# Translate column names
colnames(df) <- transMatrix[c("time","assessment","interpretation","recommendation", "warning"), lang]


# Render table

dfRendered = df %>% 
              datatable(options = list(order = list(list(1, 'desc')), pageLength = 100, columnDefs = list(list(className = 'dt-center', targets = transMatrix[c("warning"), lang]))),
                        escape = FALSE) %>% 
            #  formatStyle(valueColumns = transMatrix["warning", lang], # https://rstudio.github.io/DT/010-style.html
            #              columns = c(transMatrix["warning", lang]), 
            #              backgroundColor = styleEqual(c(TRUE, FALSE), c('red', 'green')),
            #              color = styleEqual(c(TRUE, FALSE), c('red', 'green'))
            #        ) %>% 
              renderDT() 

if (render){return(dfRendered)} else {return(df)} 
  
}