how.many.fit <- function(DATA_INPUT,input_country,MARGIN_TOLERANCE){
  
  
  #change the MARGIN_TOLERANCE so that we have plus/minus of each value
  sup_inf_data <- rbind(eng_res,den_res,sen_res)
  sup_inf_data[!is.na(sup_inf_data$inf),]$inf <- sup_inf_data[!is.na(sup_inf_data$inf),]$inf  - MARGIN_TOLERANCE
  sup_inf_data[sup_inf_data$inf<=0 & !is.na(sup_inf_data$inf),]$inf <- 0
  sup_inf_data[!is.na(sup_inf_data$sup),]$sup <- sup_inf_data[!is.na(sup_inf_data$sup),]$sup  + MARGIN_TOLERANCE
  
  sup_inf_data<-sup_inf_data[!is.na(sup_inf_data$sup),]
  
  #ALL HUMAN DATA
  for(i in 1:nrow(sup_inf_data[sup_inf_data$var=="H" & sup_inf_data$country==input_country,])){
    
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="H" & sup_inf_data$country==input_country,][i,]$time,".H",sep=""))))<= sup_inf_data$sup[sup_inf_data$var=="H" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="H" & sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="H" & sup_inf_data$country==input_country,][i,]$time,".H",sep=""))))>= sup_inf_data$inf[sup_inf_data$var=="H" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="H" & sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    
  }
  #ALL ANIMAL DATA
  for(i in 1:nrow(sup_inf_data[sup_inf_data$var=="A" & sup_inf_data$country==input_country,])){
    
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="A" & sup_inf_data$country==input_country,][i,]$time,".A",sep=""))))<= sup_inf_data$sup[sup_inf_data$var=="A" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="A" &sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="A" & sup_inf_data$country==input_country,][i,]$time,".A",sep=""))))>= sup_inf_data$inf[sup_inf_data$var=="A" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="A" &sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    
  }
  
  #ALL ENVIRONMENT DATA
  for(i in 1:nrow(sup_inf_data[sup_inf_data$var=="E" & sup_inf_data$country==input_country,])){
    
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="E" & sup_inf_data$country==input_country,][i,]$time,".E",sep=""))))<= sup_inf_data$sup[sup_inf_data$var=="E" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="E" &sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    DATA_INPUT <- DATA_INPUT[eval(parse(text=(paste0("DATA_INPUT$","model",
                                                     sup_inf_data[sup_inf_data$var=="E" & sup_inf_data$country==input_country,][i,]$time,".E",sep=""))))>= sup_inf_data$inf[sup_inf_data$var=="E" &sup_inf_data$time==sup_inf_data[sup_inf_data$var=="E" &sup_inf_data$country==input_country,][i,]$time &sup_inf_data$country==input_country ], ]
    
  }
  
  return(DATA_INPUT)
  
  
}
