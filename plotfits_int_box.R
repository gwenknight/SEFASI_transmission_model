plotfits_int_box <- function(SIR.unique,input_country){
  for(j in (1:19)){
    for(i in (1:nrow(SIR.unique))){
      assign(paste("int",j,"plotint", i, sep = ""), 
             #pmap(c(SIR.unique[i,"LAMBDA_H":"epsilon"],returnout=0,intervention=j,input_country),epid_intervention)[[1]]
             pmap(c(SIR.unique[1,] %>% select(c("LAMBDA_H":"epsilon")),returnout=0,intervention=j,input_country),epid_intervention)[[1]]
      )}
    
    tempdataint<- rep(0,nrow(SIR.unique))
    tempdataintB<- rep(0,nrow(SIR.unique))
    for (i in 1:nrow(SIR.unique)) {
      tempval <- as.list(eval(parse( text=paste("int",j,"plotint", i, sep = "") )))$differenceH_percent
      tempdataint[i] <- tempval
      tempvalB <- as.list(eval(parse( text=paste("int",j,"plotint", i, sep = "") )))$differenceH
      tempdataintB[i] <-tempvalB
    }
    
    assign( paste("dataint", j, sep = ""),t(tempdataint))
    assign( paste("dataintB", j, sep = ""),t(tempdataintB))
    
  }

  #% difference
  DATA<- data.frame(HU0=t(dataint1),AU0=t(dataint2),EU0=t(dataint3),
                    HH0=t(dataint4),AA0=t(dataint5),HE0=t(dataint6),
                    AH0=t(dataint7),EH0=t(dataint8),HA0=t(dataint9),
                    EA0=t(dataint10),AE0=t(dataint11),EE0=t(dataint12),
                    HH90=t(dataint13),HH75_HU75=t(dataint14),
                    HU80_AU70_ALLT80=t(dataint15), HU80_AU70_ALLT70=t(dataint16),
                    EH50_EA50=t(dataint17),HA50_AH50=t(dataint18),
                    HU50_AU50_EU50=t(dataint19))
  
 #absolute difference
  DATAB<- data.frame(HU0=t(dataintB1),AU0=t(dataintB2),EU0=t(dataintB3),
                    HH0=t(dataintB4),AA0=t(dataintB5),HE0=t(dataintB6),
                    AH0=t(dataintB7),EH0=t(dataintB8),HA0=t(dataintB9),
                    EA0=t(dataintB10),AE0=t(dataintB11),EE0=t(dataintB12),
                    HH90=t(dataintB13),HH75_HU75=t(dataintB14),
                    HU80_AU70_ALLT80=t(dataintB15), HU80_AU70_ALLT70=t(dataintB16),
                    EH50_EA50=t(dataintB17),HA50_AH50=t(dataintB18),
                    HU50_AU50_EU50=t(dataintB19))
  
  DATA<- reshape2::melt(DATA)
  DATAB<- reshape2::melt(DATAB)
  #print(DATA)
  
  #percent difference
  P1<- ggplot(DATA)+ geom_boxplot(aes(x=factor(variable),y=value*100,fill=factor(variable)))+
    ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
    xlab(label="Intervention")+ 
    scale_fill_manual(labels = c('HU0',
                                 'AU0',
                                 'EU0',
                                 'HH0',
                                 'AA0',
                                 'HE0',
                                 'AH0',
                                 'EH0',
                                 'HA0',
                                 'EA0',
                                 'AE0',
                                 'EE0',
                                 'HH90',
                                 'HH75_HU75',
                                 'HU80_AU70_ALLT80',
                                 'HU80_AU70_ALLT70',
                                 'EH50_EA50',
                                 'HA50_AH50',
                                 'HU50_AU50_EU50'
    ),values=cols)+
    # 'no decay of AB-resistance in humans',
    # 'no decay of AB-resistance in animals',
    # 'no decay of AB-resistance in environment'))+
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
    theme(axis.text.x = element_text(color="black", 
                                     size=15),
          axis.text.y = element_text(color="black", 
                                     size=15)) + theme(legend.title = element_blank()) +
    #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
    scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
    
    theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())+
    scale_y_continuous(limits=c(0,100),expand = c(0,0)) 
  
 
#  print(paste("~/Documents/Projects/SEFASI/outputs/INT_DATA_",input_country, ".csv",sep=""))
  fwrite(DATA,paste("~/Documents/Projects/SEFASI/outputs/INT_DATA_",input_country, ".csv",sep=""))
  DATA <- DATA %>% group_by(variable)
  SUMMM<-DATA%>% summarise(
    median=round(median(value),3)*100 ,
    quantilelower=round(quantile(value, c(0.025, 0.975)) ,3)[[1]]*100,
    quantileupper=round(quantile(value, c(0.025, 0.975)) ,3)[[2]]*100)
    fwrite(SUMMM,paste("~/Documents/Projects/SEFASI/outputs/INT_summary_",input_country, ".csv",sep=""))
    
    
grid.arrange(P1) 
 ggsave(paste("~/Documents/Projects/SEFASI/outputs/INT_box_",input_country, ".png",sep=""), width = 400, height = 300, units='mm',dpi=1000)
  
 #ABSOLUTE DIFFERENCE
 
 P2<- ggplot(DATAB)+ geom_boxplot(aes(x=factor(variable),y=value*100,fill=factor(variable)))+
   ylab(label="Absolute reduction in resistant bacteria in humans over 20 years")+
   xlab(label="Intervention")+ 
   scale_fill_manual(labels = c('HU0',
                                'AU0',
                                'EU0',
                                'HH0',
                                'AA0',
                                'HE0',
                                'AH0',
                                'EH0',
                                'HA0',
                                'EA0',
                                'AE0',
                                'EE0',
                                'HH90',
                                'HH75_HU75',
                                'HU80_AU70_ALLT80',
                                'HU80_AU70_ALLT70',
                                'EH50_EA50',
                                'HA50_AH50',
                                'HU50_AU50_EU50'
   ),values=cols)+
   # 'no decay of AB-resistance in humans',
   # 'no decay of AB-resistance in animals',
   # 'no decay of AB-resistance in environment'))+
   theme_classic() +
   theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
   theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
   theme(axis.text.x = element_text(color="black", 
                                    size=15),
         axis.text.y = element_text(color="black", 
                                    size=15)) + theme(legend.title = element_blank()) +
   #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
   scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
   
   theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
   theme(
     axis.text.x=element_blank(),
     axis.ticks.x=element_blank())+
   scale_y_continuous(limits=c(0,100),expand = c(0,0)) 
 
 
 #  print(paste("~/Documents/Projects/SEFASI/outputs/INT_DATA_",input_country, ".csv",sep=""))
 fwrite(DATAB,paste("~/Documents/Projects/SEFASI/outputs/INT_DATA_abs_",input_country, ".csv",sep=""))
 DATAB <- DATAB %>% group_by(variable)
 SUMMM<-DATAB%>% summarise(
   median=round(median(value),3)*100 ,
   quantilelower=round(quantile(value, c(0.025, 0.975)) ,3)[[1]]*100,
   quantileupper=round(quantile(value, c(0.025, 0.975)) ,3)[[2]]*100)
 fwrite(SUMMM,paste("~/Documents/Projects/SEFASI/outputs/INT_summary_abs_",input_country, ".csv",sep=""))
 
 
 grid.arrange(P2) 
 ggsave(paste("~/Documents/Projects/SEFASI/outputs/INT_box_abs_",input_country, ".png",sep=""), width = 400, height = 300, units='mm',dpi=1000)
 
 
 
  #return(DATA) #return the dataframe to find 95CI% and median below
 
}
