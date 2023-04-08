severityPlot = function(scales, cutoffs, TimeOnXAxis = TRUE){
 


  if(nrow(scales) < 1){return(NULL)}
    

  myplots = vector('list', scales$group %>% unique() %>% length())
  
  scales = scales %>% mutate(group = ifelse(is.na(group), scale %>% factor() %>% as.numeric(), group ))
  
  
  for (g in scales$group %>% unique){
    
    message(g)
    myplots[[g]] <- local({ # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
      g <- g  
    
  s = scales %>% filter(group == g)  %>% arrange(scale, assessmentDateTime)
  c = cutoffs %>% filter(group == g) 
  
  c$level = c$level %>% factor
  c$level = fct_reorder(c$level, c$low_cut)
  
  dfDummy =  s %>% # dummy data to set the limits of the y-axis
    mutate(value  = ifelse(is.na(scaleMax), value, scaleMax)) %>% 
    bind_rows(s %>% mutate(value = ifelse(is.na(scaleMin), value, scaleMin)))
  
  
  if (length(unique(s$assessmentDateTime)) == 1) {
    p = ggplot() +
      geom_blank(data = dfDummy, aes(x=scale, y=value))
    
    if(any(!is.na(c$low_cut)) & any(!is.na(c$high_cut))){
    p = p + 
      geom_rect(data = c, aes(xmin = -Inf, xmax = Inf, ymin = low_cut, ymax = high_cut, fill = level ), alpha = 0.4)
    } 
    
    
    p = p +   
      geom_segment(data = s, aes(x=scale, xend=scale, y=scaleMin, yend=value)) + 
      geom_point(data = s, aes(x=scale, y=value),size=5, color="darkblue", fill=alpha("white", 0.3), alpha=0.7, shape=21, stroke=2) +
      ggtitle(paste(s$assessmentDateTime, s$assessmentName, sep = " - ")) 
    

    
  } else {
    
    if(TimeOnXAxis){ 
      
      p =  ggplot() +
        geom_blank(data = dfDummy, aes(x = assessmentDate, y = value, colour = scale, group = scale)) +
        xlab("Assessment") 
      

     

      
      if(any(!is.na(c$low_cut)) & any(!is.na(c$high_cut))){
        p = p +
          geom_rect(data = c,aes(xmin = min(dfDummy$assessmentDate) -5,
                        xmax = max(dfDummy$assessmentDate) +5 ,
                        ymin = low_cut,
                        ymax = high_cut,
                        fill = level),
                     alpha = 0.9,
                     colour = "white"
                    )   
      
      }
      } else {return(NULL)  }
    
      p = p + 
        geom_point(data = s,  
                   aes(x = assessmentDate, y = value, colour = scale),
                   size=3.5,
                   fill=alpha("white", 1),
                   alpha=0.7,
                   shape=21,
                   stroke=2,
                   position = position_jitter(width = .3, height = 0, seed = 1)) 
      
      
      p = p + 
        geom_line(data = s,  
                  aes(x = assessmentDate, y = value, colour = scale, linetype = scale),
                  alpha = 0.5, 
                  lwd = 2,
                  position = position_jitter(width = .3, height = 0, seed = 1)) 
      

  
     }
    
    
  p = p +  
    theme_light() +
    #scale_colour_grey() +
    scale_colour_brewer(type = "div", palette = "Dark2", direction = 1) +
    scale_fill_brewer(type = "seq", palette = "Reds", direction = 1) +
    ylab("value") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylim(c(min(s$scaleMin), max(s$scaleMax)))
  
 p 
 
  })
  
  }
  
  myplots = myplots %>% compact()
  
  return(myplots)
  
} 