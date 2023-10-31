
res_H_denmark <- res.table[res.table$country=="denmark" & res.table$var=="H",]
res_A_denmark <- res.table[res.table$country=="denmark" & res.table$var=="A",]
res_E_denmark <- res.table[res.table$country=="denmark" & res.table$var=="E",]

LL_H_denmark <- rep(NA,nrow(res_H_denmark))
LL_A_denmark <- rep(NA,nrow(res_A_denmark))
LL_E_denmark <- rep(NA,nrow(res_E_denmark))

times_H_denmark <- res_H_denmark$time
times_A_denmark <- res_A_denmark$time
times_E_denmark <- res_E_denmark$time

LL_simple_denmark  <- function(DATA_INPUT){
  
  for (i in 1:length(LL_H_denmark)) {
    LL_H_denmark[i] = res_H_denmark$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H_denmark[i],".H",sep="")))) + (1- res_H_denmark$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H_denmark[i],".H",sep=""))))   
  }
  
  for (i in 1:length(LL_A_denmark)) {
    LL_A_denmark[i] = res_A_denmark$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_denmark[i],".A",sep="")))) + (1- res_A_denmark$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_denmark[i],".A",sep=""))))   
  }
  
  for (i in 1:length(LL_E_denmark)) {
    LL_E_denmark[i] = res_E_denmark$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_denmark[i],".E",sep="")))) + (1- res_E_denmark$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_denmark[i],".E",sep=""))))   
  }
  
  
  MLE<- sum(LL_H_denmark)/length(LL_H_denmark) + sum(LL_A_denmark)/length(LL_A_denmark) +  sum(LL_E_denmark)/length(LL_E_denmark)
  #print(MLE)
  
  return(MLE)
}
