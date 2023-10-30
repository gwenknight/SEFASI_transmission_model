

###ENGLAND

fittedvalue<-1
LAMBDA_H_temp_input <- fittedvalue
H_A_ratio <- initial_eng_H/initial_eng_A
LAMBDA_time <- function(max_time){
  temp_time_H <- rep(NA,max_time)
  temp_time_A <- rep(NA,max_time)
  for (i in c(1:max_time) ){
    if (i <= time1_eng)  {LAMBDA_H_temp <- LAMBDA_H_temp_input
    LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio } else if ((i >time1_eng) & (i < time2_eng))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input + ((i-time1_eng)*(((LAMBDA_H_temp_input*ratio_eng_2017_H) - LAMBDA_H_temp_input)/(time2_eng-time1_eng)))
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio + ((i-time1_eng)*(((LAMBDA_H_temp_input/H_A_ratio)*ratio_eng_2017_A - LAMBDA_H_temp_input/H_A_ratio )/(time2_eng-time1_eng)))} else if (i>=time2_eng) { 
        LAMBDA_H_temp <- LAMBDA_H_temp_input*ratio_eng_2017_H
        LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio)*ratio_eng_2017_A}
    
    temp_time_H[i] <- LAMBDA_H_temp
    temp_time_A[i] <- LAMBDA_A_temp
  }
  
  return(c(temp_time_H,temp_time_A))
}

H_data<- data.frame(LAMBDA_time(20)[1:20])
A_data <- data.frame(LAMBDA_time(20)[21:40])
colnames(H_data) <- "H"
colnames(A_data) <- "A"
H_data$time <- 2000+1:20
A_data$time <- 2000+1:20

H_data$H <- initial_eng_H*H_data$H 
A_data$A <- initial_eng_H*A_data$A
#ggplot(as.data.frame(H_data))+geom_point(data=as.data.frame(H_data),aes(time,H),col="lightblue")+geom_point(data=as.data.frame(A_data),aes(time,A))+
# theme(panel.border = element_rect(linetype = "solid", fill = NA))#+

ggplot(data=usage.table[usage.table$country=="england" ,], aes(x=time))+
  geom_point(data=usage.table[usage.table$country=="england" ,],aes(x=year,y=kg,colour=source),shape="square",size=4,alpha=0.8) +
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
  geom_point(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_point(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#+
 # geom_line(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_line(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#
#scale_x_continuous(expand = c(2000, 2020),limits=c(2000,2020)) + scale_y_continuous(expand = c(0, 0))
ggsave(paste("plots/time_varying_usage_england", ".png",sep=""), width = 150, height = 100, units='mm',dpi=1000)


fittedvalue<-1
LAMBDA_H_temp_input <- fittedvalue
H_A_ratio <- initial_den_H/initial_den_A

LAMBDA_time <- function(max_time){
  temp_time_H <- rep(NA,max_time)
  temp_time_A <- rep(NA,max_time)
  for (i in c(1:max_time) ){
    if (i <= 2)  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio_den } else if ((i >2) & (i <= 3))  {
        LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
        LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A} else if ((i >3) & (i <= 15))  {  
          LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
          LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A + (i-3)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2015_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A)/(15-3)) } else if ((i >15) & (i <= 16))  {  
            LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
            LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A}  else if ((i>16) & (i <= 18))   { 
              LAMBDA_H_temp<- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
              LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A + (i-16)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A)/(18-16))} else if ((i >18) & (i <= 20))  {
                LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2000+i),]$kg/initial_den_H
                LAMBDA_A_temp <-   (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A + (i-18)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2020_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A)/(20-18)) } else {
                LAMBDA_H_temp <- LAMBDA_H_temp_input* den_usage[den_usage$year == (2019),]$kg/initial_den_H 
                LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2021_A }
    
    temp_time_H[i] <- LAMBDA_H_temp
    temp_time_A[i] <- LAMBDA_A_temp
  }
  
  return(c(temp_time_H,temp_time_A))
}

H_data<- data.frame(LAMBDA_time(25)[1:25])
A_data <- data.frame(LAMBDA_time(25)[26:50])
colnames(H_data) <- "H"
colnames(A_data) <- "A"
H_data$time <- 2000+1:25
A_data$time <- 2000+1:25

H_data$H <- initial_den_H*H_data$H 
A_data$A <- initial_den_H*A_data$A
ggplot(data=usage.table[usage.table$country=="denmark" & usage.table$subsource %in% c("all animals","all humans")  ,], aes(x=time))+
  geom_point(data=usage.table[usage.table$country=="denmark" &  (usage.table$subsource %in% c("all animals","all humans")) ,],aes(x=year,y=kg,colour=source),shape="square",size=4,alpha=0.8) +
 # geom_point(data=usage.table[usage.table$country=="denmark" ,],aes(x=year,y=kg,colour=source),shape="triangle",size=1,alpha=0.8) +
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
  geom_point(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_point(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#+
 # geom_line(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_line(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#+
ggsave(paste("plots/time_varying_usage_denmark", ".png",sep=""), width = 150, height = 100, units='mm',dpi=1000)

