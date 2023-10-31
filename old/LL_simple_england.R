
res_H_england <- res.table[res.table$country=="england" & res.table$var=="H",]
res_A_england <- res.table[res.table$country=="england" & res.table$var=="A",]
res_E_england <- res.table[res.table$country=="england" & res.table$var=="E",]

LL_H_england <- rep(NA,nrow(res_H_england))
LL_A_england <- rep(NA,nrow(res_A_england))
LL_E_england <- rep(NA,nrow(res_E_england))

times_H_england <- res_H_england$time
times_A_england <- res_A_england$time
times_E_england <- res_E_england$time

LL_simple_england  <- function(DATA_INPUT){
  
  for (i in 1:length(LL_H_england)) {
    LL_H_england[i] = res_H_england$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H_england[i],".H",sep="")))) + (1- res_H_england$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H_england[i],".H",sep=""))))   
  }
  
  for (i in 1:length(LL_A_england)) {
    LL_A_england[i] = res_A_england$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_england[i],".A",sep="")))) + (1- res_A_england$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_england[i],".A",sep=""))))   
  }
  
  for (i in 1:length(LL_E_england)) {
    LL_E_england[i] = res_E_england$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_england[i],".E",sep="")))) + (1- res_E_england$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_england[i],".E",sep=""))))   
  }
  
  
  MLE<- sum(LL_H_england)/length(LL_H_england) + sum(LL_A_england)/length(LL_A_england) +  sum(LL_E_england)/length(LL_E_england)
  #print(MLE)
  
  return(MLE)
}
