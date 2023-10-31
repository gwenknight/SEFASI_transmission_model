
res_H_senegal <- res.table[res.table$country=="senegal" & res.table$var=="H",]
res_A_senegal <- res.table[res.table$country=="senegal" & res.table$var=="A",]
res_E_senegal <- res.table[res.table$country=="senegal" & res.table$var=="E",]

LL_H_senegal <- rep(NA,nrow(res_H_senegal))
LL_A_senegal <- rep(NA,nrow(res_A_senegal))
LL_E_senegal <- rep(NA,nrow(res_E_senegal))

times_H_senegal <- res_H_senegal$time
times_A_senegal <- res_A_senegal$time
times_E_senegal <- res_E_senegal$time

LL_simple_senegal  <- function(DATA_INPUT){

    for (i in 1:length(LL_H_senegal)) {
      LL_H_senegal[i] = res_H_senegal$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H_senegal[i],".H",sep="")))) + (1- res_H_senegal$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H_senegal[i],".H",sep=""))))   
    }
    
    for (i in 1:length(LL_A_senegal)) {
      LL_A_senegal[i] = res_A_senegal$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_senegal[i],".A",sep="")))) + (1- res_A_senegal$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_senegal[i],".A",sep=""))))   
    }
    
    for (i in 1:length(LL_E_senegal)) {
      LL_E_senegal[i] = res_E_senegal$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_senegal[i],".E",sep="")))) + (1- res_E_senegal$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_senegal[i],".E",sep=""))))   
    }
    
    
   MLE<- sum(LL_H_senegal)/length(LL_H_senegal) + sum(LL_A_senegal)/length(LL_A_senegal) +  sum(LL_E_senegal)/length(LL_E_senegal)
    #print(MLE)
  
  return(MLE)
}
