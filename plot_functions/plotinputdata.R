### Plots the resistance data from the literature

plot.res.table<- ggplot(data=res.table, aes(x=time))+
  geom_point(data=res.table,aes(x=time,y=percent*100,shape=subsource),size=4,alpha=1) +
  scale_color_brewer(palette = "Set1")+
  ylab(label="% E. coli resistant to 3GCP")+
  xlab(label="Year")+ 
  scale_y_continuous(limits = c(0, 100)) +
  #scale_x_continuous(limits = c(epid.start, max(res.table$time)+1)) +
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
 # scale_color_manual(name="",values = c("H" = "#3B9AB2","A" = "darkgreen", "E" ="#EBCC2A" ))+
  theme(legend.position = 'right')+guides(shape=guide_legend(ncol=1))+
  #geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.1,fill = "darkgreen",size=0.5)+
  #geom_ribbon(data=dataE,aes(ymin=dataE$lower*100, ymax=dataE$upper*100), linetype=0, alpha=0.1,fill = "#EBCC2A",size=0.5)+
  #geom_ribbon(data=dataH,aes(ymin=dataH$lower*100, ymax=dataH$upper*100), linetype=0, alpha=0.1,fill = "#3B9AB2",size=0.5)+
  
 # geom_errorbar(data=res.table[res.table$var =="H",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("H")), width=0.5,size=1) +
#  geom_errorbar(data=res.table[res.table$var =="A",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("A")), width=0.5,size=1) +
#  geom_errorbar(data=res.table[res.table$var =="E",],aes(x=time,ymin=inf*100, ymax=sup*100,colour=c("E")), width=0.5,size=1) +
  geom_errorbar(data=sup_inf_data,aes(x=time,ymin=inf*100, ymax=sup*100), width=0.5,size=1,alpha=0.25,colour="blue") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
facet_wrap(. ~ country+var, scales="fixed",ncol=3)+
  scale_shape_manual(values=seq(0,21))
ggsave("plots/plot.res.table.png", width = 300, height = 250, units='mm',dpi=1000)

plot.usage.table<- ggplot(data=usage.table, aes(x=time))+
  geom_point(data=usage.table,aes(x=year,y=kg,colour=source,shape=subsource),size=1.5,alpha=0.8) +
  scale_color_brewer(palette = "Set1")+
  ylab(label="Usage in KG")+
  xlab(label="Year")+ 
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
  scale_color_manual(name="",values = c("humans" = "#3B9AB2","animals" = "darkgreen", "environment" ="#EBCC2A" ))+
  theme(legend.position = 'right')+guides(colour=guide_legend(ncol=1))+
   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+

  #geom_ribbon(data=dataA,aes(ymin=dataA$lower*100, ymax=dataA$upper*100), linetype=0, alpha=0.1,fill = "darkgreen",size=0.5)+
  #geom_ribbon(data=dataE,aes(ymin=dataE$lower*100, ymax=dataE$upper*100), linetype=0, alpha=0.1,fill = "#EBCC2A",size=0.5)+
  #geom_ribbon(data=dataH,aes(ymin=dataH$lower*100, ymax=dataH$upper*100), linetype=0, alpha=0.1,fill = "#3B9AB2",size=0.5)+
  
  facet_wrap(. ~ country, scales="fixed",ncol=3)
ggsave("plots/plot.usage.table.png", width = 250, height = 100, units='mm',dpi=1000)
