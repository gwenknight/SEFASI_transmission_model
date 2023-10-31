###### Plots the data 


plotfits_int <- function(SIR.unique,input_country,MARGIN_TOLERANCE, sup_inf_data){
  
  #change the MARGIN_TOLERANCE so that we have plus/minus of each value
  sup_inf_data[!is.na(sup_inf_data$inf),]$inf <- sup_inf_data[!is.na(sup_inf_data$inf),]$inf  - MARGIN_TOLERANCE
  sup_inf_data[sup_inf_data$inf<=0 & !is.na(sup_inf_data$inf),]$inf <- 0
  sup_inf_data[!is.na(sup_inf_data$sup),]$sup <- sup_inf_data[!is.na(sup_inf_data$sup),]$sup  + MARGIN_TOLERANCE
  
  sup_inf_data<-sup_inf_data[!is.na(sup_inf_data$sup),]
  
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plot", i, sep = ""), 
           epid(params = SIR.unique[i,LAMBDA_H:epsilon],returnout=1,input_country)
           #pmap(c(params = SIR.unique[i,LAMBDA_H:epsilon],returnout=1,input_country),epid)[[1]]
           #intervention = 1)
    )
  }
  
  dataH=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataH[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$H 
  }
  
  
  datatemp <- dataH[,2:ncol(dataH)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataH$median<- datatemp$median
  dataH$lower<- datatemp$lower
  dataH$upper<- datatemp$upper
  
  dataA=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataA[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A
  }
  
  
  datatemp <- dataA[,2:ncol(dataA)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataA$median<- datatemp$median
  dataA$lower<- datatemp$lower
  dataA$upper<- datatemp$upper
  
  dataE=data.frame(time=plot1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataE[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$E 
  }
  
  
  datatemp <- dataE[,2:ncol(dataE)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataE$median<- datatemp$median
  dataE$lower<- datatemp$lower
  dataE$upper<- datatemp$upper
  
  
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plotint", i, sep = ""), 
           epid_intervention(LAMBDA_H=SIR.unique[[i,1]],LAMBDA_A=SIR.unique[[i,2]],LAMBDA_E=SIR.unique[[i,3]],
                             SIR.unique[[i,4]], SIR.unique[[i,5]],
                             SIR.unique[[i,6]], SIR.unique[[i,7]],
                             SIR.unique[[i,8]], SIR.unique[[i,9]],
                             SIR.unique[[i,10]], SIR.unique[[i,11]],
                             SIR.unique[[i,12]],
                             SIR.unique[[i,13]], SIR.unique[[i,14]],SIR.unique[[i,15]],
                             SIR.unique[[i,16]], 1, SIR.unique[[i,17]],
                             intervention = 1,input_country = input_country))}
  
  dataHint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataHint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$H 
  }
  
  
  datatemp <- dataHint[,2:ncol(dataHint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataHint$median<- datatemp$median
  dataHint$lower<- datatemp$lower
  dataHint$upper<- datatemp$upper
  
  dataAint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataAint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$A
  }
  
  
  datatemp <- dataAint[,2:ncol(dataAint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataAint$median<- datatemp$median
  dataAint$lower<- datatemp$lower
  dataAint$upper<- datatemp$upper
  
  dataEint=data.frame(time=plotint1$time)
  for (i in 1:nrow(SIR.unique)) {
    dataEint[,1+i] <- eval(parse( text=paste("plotint", i, sep = "") ))$E 
  }
  
  
  datatemp <- dataEint[,2:ncol(dataEint)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataEint$median<- datatemp$median
  dataEint$lower<- datatemp$lower
  dataEint$upper<- datatemp$upper
  
  
  P1_H<-ggplot(data=dataH, aes(x=time))+
    geom_line(data=dataH,aes(x=time,y=median*100,colour="Humans"),size=1.5,alpha=1) +
    geom_point(data=res.table[res.table$country==input_country &res.table$var=="H",],aes(x=time,y=percent*100),size=2,alpha=0.25) +
    scale_color_brewer(palette = "Set1")+
    ylab(label="% samples with resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(epid.start, end.time+1)) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) +
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataH,aes(ymin=dataH$lower*100, ymax=dataH$upper*100), linetype=0, alpha=0.1,fill = "#3B9AB2",size=0.5)+
    geom_errorbar(data=sup_inf_data[sup_inf_data$var =="H"&sup_inf_data$country==input_country,],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Humans")), width=0.5,size=1,alpha=0.5)+
    geom_line(data=dataHint,aes(x=time,y=median*100),colour="black",size=1.5,alpha=0.3)+
    geom_ribbon(data=dataHint,aes(ymin=dataHint$lower*100, ymax=dataHint$upper*100), linetype=0, alpha=0.1,fill = "grey",size=0.5)
  
  P1_A<-ggplot(data=dataA, aes(x=time))+
    geom_line(data=dataA,aes(x=time,y=median*100,colour="Animals"),size=1.5,alpha=1) +
    geom_point(data=res.table[res.table$country==input_country &res.table$var=="A",],aes(x=time,y=percent*100),size=2,alpha=0.25) +
    scale_color_brewer(palette = "Set1")+
    ylab(label="% samples with resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(epid.start, end.time+1)) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) +
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.1,fill = "#3B9AB2",size=0.5)+
    geom_errorbar(data=sup_inf_data[sup_inf_data$var =="A"&sup_inf_data$country==input_country,],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Animals")), width=0.5,size=1,alpha=0.5)+
    geom_line(data=dataAint,aes(x=time,y=median*100,colour="black"),size=1.5,alpha=0.3)+
    geom_ribbon(data=dataAint,aes(ymin=dataHint$lower*100, ymax=dataHint$upper*100), linetype=0, alpha=0.3,fill = "black",size=0.5)
  
  P1_A<-ggplot(data=dataA, aes(x=time))+
    geom_line(data=dataA,aes(x=time,y=median*100,colour="Animals"),size=1.5,alpha=1) +
    geom_point(data=res.table[res.table$country==input_country &res.table$var=="A",],aes(x=time,y=percent*100),size=2,alpha=0.25) +
    scale_color_brewer(palette = "Set1")+
    ylab(label="% samples with resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(epid.start, end.time+1)) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) +
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.1,fill = "darkgreen",size=0.5)+
    geom_errorbar(data=sup_inf_data[sup_inf_data$var =="A"&sup_inf_data$country==input_country,],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Animals")), width=0.5,size=1,alpha=0.5)+
    geom_line(data=dataAint,aes(x=time,y=median*100),colour="black",size=1.5,alpha=0.3)+
    geom_ribbon(data=dataAint,aes(ymin=dataAint$lower*100, ymax=dataAint$upper*100), linetype=0, alpha=0.1,fill = "grey",size=0.5)
  
  
  P1_E<-ggplot(data=dataE, aes(x=time))+
    geom_line(data=dataE,aes(x=time,y=median*100,colour="Environment"),size=1.5,alpha=1) +
    geom_point(data=res.table[res.table$country==input_country &res.table$var=="E",],aes(x=time,y=percent*100),size=2,alpha=0.25) +
    scale_color_brewer(palette = "Set1")+
    ylab(label="% samples with resistant bacteria")+
    xlab(label="Year")+ 
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(epid.start, max(res.table$time)+1)) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) +
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
    geom_ribbon(data=dataE,aes(ymin=dataE$lower*100, ymax=dataE$upper*100), linetype=0, alpha=0.1,fill = "#EBCC2A",size=0.5)+
    geom_errorbar(data=sup_inf_data[sup_inf_data$var =="E"&sup_inf_data$country==input_country,],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("Environment")), width=0.5,size=1,alpha=0.5)+
    geom_line(data=dataEint,aes(x=time,y=median*100),colour="black",size=1.5,alpha=0.3)+
    geom_ribbon(data=dataEint,aes(ymin=dataEint$lower*100, ymax=dataEint$upper*100), linetype=0, alpha=0.1,fill = "grey",size=0.5)
  
  print(plot_grid(P1_H+ theme(legend.position = 'none'), P1_A+ theme(legend.position = 'none'), P1_E+ theme(legend.position = 'none'), ncol=3, align = "v"))
  
  ggsave(paste("plots/plot.fits.int.",input_country, ".png",sep=""), width = 500, height = 100, units='mm',dpi=1000)
}
