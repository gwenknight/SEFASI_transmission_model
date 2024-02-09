####### How far is the model output from the data? ###########


### Log likelihood simple for each row of data


### Least squares
LS_simple  <- function(DATA_INPUT, input_country, res.table){
  
  # Extract the resistance prevalence data for this country 
  res_H_country <- res.table[res.table$country==input_country & res.table$var=="H",]
  res_A_country <- res.table[res.table$country==input_country & res.table$var=="A",]
  res_E_country <- res.table[res.table$country==input_country & res.table$var=="E",]
  
  # Generate empty matrix to store LL
  LL_H_country <- rep(NA,nrow(res_H_country))
  LL_A_country <- rep(NA,nrow(res_A_country))
  LL_E_country <- rep(NA,nrow(res_E_country))
  
  # At what times are there data to compare to? 
  times_H_country <- res_H_country$time
  times_A_country <- res_A_country$time
  times_E_country <- res_E_country$time
  
  # For the datapoints check how far model from data: GK ERROR here: res_H_country no necessarily correct year! 
  for (i in 1:length(LL_H_country)) {
    #LL_H_country[i] = res_H_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep="")))) + 
    #  (1- res_H_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))   
    # Least squares
    LL_H_country[i] = (res_H_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))^2
    
  }
  
  for (i in 1:length(LL_A_country)) {
    #LL_A_country[i] = res_A_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep="")))) + (1- res_A_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))   
    
    # Least squares
    LL_A_country[i] = (res_A_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))^2
  }
  
  for (i in 1:length(LL_E_country)) {
    #LL_E_country[i] = res_E_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep="")))) + (1- res_E_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))   
    # Least squares
    LL_E_country[i] = (res_E_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))^2
  }
  
  
  #MLE<- sum(LL_H_country)/length(LL_H_country) + sum(LL_A_country)/length(LL_A_country) +  sum(LL_E_country)/length(LL_E_country)
  LS <- sum(LL_H_country) + sum(LL_A_country) +  sum(LL_E_country)
  #print(MLE)
  
  return(LS)
}

### Overview Log likelihood function
LL_function  <- function(DATA_INPUT,input_country){
  
  res_H <- res.table[res.table$country==input_country & res.table$var=="H",]
  res_A <- res.table[res.table$country==input_country & res.table$var=="A",]
  res_E <- res.table[res.table$country==input_country & res.table$var=="E",]
  
  LL_H <- rep(NA,nrow(res_H))
  LL_A <- rep(NA,nrow(res_A))
  LL_E <- rep(NA,nrow(res_E))
  
  times_H <- res_H$time
  times_A <- res_A$time
  times_E <- res_E$time
  
  for (i in 1:length(LL_H)) {
    LL_H[i] = res_H$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H[i],".H",sep="")))) + (1- res_H$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H[i],".H",sep="")))) +  
      log(chooseZ(1,res_H$percent[i]))
  }
  
  for (i in 1:length(LL_A)) {
    LL_A[i] = res_A$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A[i],".A",sep="")))) + (1- res_A$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A[i],".A",sep="")))) +  
      log(chooseZ(1,res_A$percent[i]))
  }
  
  for (i in 1:length(LL_E)) {
    LL_E[i] = res_E$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E[i],".E",sep="")))) + (1- res_E$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E[i],".E",sep="")))) +  
      log(chooseZ(1,res_E$percent[i]))
  }
  LL_H_weight <- sum(LL_H)/length(LL_H)
  LL_A_weight <- sum(LL_A)/length(LL_A)
  LL_E_weight  <- sum(LL_E)/length(LL_E)
  toReturn <- as.vector(c(LL_H,LL_A,LL_E,LL_H_weight,LL_A_weight,LL_E_weight))
  
  names(toReturn) <-  
    c(paste0("LL_H", 1: length(LL_H)),
      paste0("LL_A", 1: length(LL_A)),
      paste0("LL_E", 1: length(LL_E)),
      "LL_H_weight","LL_A_weight","LL_E_weight"
    )
  
  
  return(toReturn)
  
}
