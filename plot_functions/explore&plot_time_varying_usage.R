################## Explore and plot time varying antibiotic usage ###### 
#### Antibiotic usage
usage.table <- as.data.frame(read.csv("data/usage.csv"))

###################################################ENGLAND
### Raw data
eng_use <- usage.table %>% filter(country == "england")

# initial values for abx use
initial_eng_H = eng_use %>% filter(source == "humans", year == min(eng_use$year)) %>% select("kg") #3400
initial_eng_A = eng_use %>% filter(source == "animals", year == min(eng_use$year)) %>% select("kg") #1200

# switch points in English data: assume flat before and after with linear join between 
time1_eng<- min(eng_use$year) - 2000 #13
time2_eng<- max(eng_use$year) - 2000 #17

# Ratio of use in humans to animals 
ratio_eng_2017_H <-  eng_use %>% filter(source == "humans", year == max(eng_use$year)) %>% select("kg")/initial_eng_H
ratio_eng_2017_A <-  eng_use %>% filter(source == "animals", year == max(eng_use$year)) %>% select("kg")/initial_eng_A
H_A_ratio_eng <-initial_eng_H/initial_eng_A

# Initial value
fittedvalue<-1
LAMBDA_H_temp_input <- fittedvalue

# Function to calculate over time abx use ratios (LAMBDA): Human = 1, animal = relative to this
LAMBDA_time <- function(max_time){
  # empty initial vector
  temp_time_H <- rep(NA,max_time)
  temp_time_A <- rep(NA,max_time)
  for (i in c(1:max_time) ){
    if (i <= time1_eng)  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio_eng 
    } else if ((i >time1_eng) & (i < time2_eng))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input + ((i-time1_eng)*(((LAMBDA_H_temp_input*ratio_eng_2017_H) - LAMBDA_H_temp_input)/(time2_eng-time1_eng)))
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio_eng + ((i-time1_eng)*(((LAMBDA_H_temp_input/H_A_ratio_eng)*ratio_eng_2017_A - LAMBDA_H_temp_input/H_A_ratio_eng )/(time2_eng-time1_eng)))
    } else if (i>=time2_eng) { 
      LAMBDA_H_temp <- LAMBDA_H_temp_input*ratio_eng_2017_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_eng)*ratio_eng_2017_A
    }
    # Store the temporary one in the output vector
    temp_time_H[i] <- LAMBDA_H_temp
    temp_time_A[i] <- LAMBDA_A_temp
  }
  
  return(c(temp_time_H,temp_time_A))
}
# Calculate human levels relative to 1 initially 
H_data<- as.data.frame(t(data.frame((LAMBDA_time(20)[1:20]))))
A_data <- as.data.frame(t(data.frame(LAMBDA_time(20)[21:40]))) # relative to human levels
colnames(H_data) <- "H"
colnames(A_data) <- "A"
H_data$time <- 2000+1:20
A_data$time <- 2000+1:20

# Multiply by initial values to get back to antibiotic usage in kg
H_data$H <- as.numeric(initial_eng_H)*H_data$H 
A_data$A <- as.numeric(initial_eng_H)*A_data$A
ggplot(as.data.frame(H_data))+geom_point(data=as.data.frame(H_data),aes(time,H),col="lightblue")+geom_point(data=as.data.frame(A_data),aes(time,A))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA))#+

# Generate plot showing input and overlay data 
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
  geom_point(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_point(data=as.data.frame(A_data),aes(time,A),col="darkgreen")+
  scale_y_continuous(limits = c(0, 1.01 * max(eng_use$kg)))
# geom_line(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_line(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#
#scale_x_continuous(expand = c(2000, 2020),limits=c(2000,2020))
ggsave(paste("plots/time_varying_usage_england", ".png",sep=""), width = 150, height = 100, units='mm',dpi=1000)



#################################################### Denmark 
### Raw data
den_use <- usage.table %>% filter(country == "denmark")

#### Add in missing data for 2020 and 2000
# # ?assumption leave out 2020 point  (8.9) because not comparable to just "cephalosporins"
## Assume 2020 levels same as 2019 as no data 
temp_2020_row_extra <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans" & usage.table$year == (2019),] #this is to make sure 2020 point fits into function assumption 2020= 2019
temp_2020_row_extra$year <- 2020

#for AMRmodel.R to make sense we need to add data before 2001: assume same in 2000 as in 2001
temp_2000_row_extra <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans" & usage.table$year == (2001),] #this is to make sure 2020 point fits into function assumption 2020= 2019
temp_2000_row_extra$year <- 2000
usage.table <- rbind(usage.table,temp_2020_row_extra,temp_2000_row_extra)

den_use_h <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans",c("year","kg")]


# Break points for Denmark data
time1_den <- 2
time2_den <- 3
time3_den <- 15
time4_den <- 16
time5_den <- 18

# Start in 2002 when have data from both
initial_den_H = as.numeric(den_use %>% filter(source == "humans", year == 2002) %>% select("kg")) # 811
initial_den_A = as.numeric(den_use %>% filter(source == "animals", year == 2002) %>% select("kg")) # 385
# At critical points
ratio_den_2003_H<-  den_use %>% filter(source == "humans", year == 2003) %>% select("kg")/initial_den_H
ratio_den_2003_A<-  461/initial_den_A
ratio_den_2015_H<-  den_use %>% filter(source == "humans", year == 2015) %>% select("kg")/initial_den_H
ratio_den_2015_A<-  235/initial_den_A
ratio_den_2016_H<-  den_use %>% filter(source == "humans", year == 2016) %>% select("kg")/initial_den_H
ratio_den_2016_A<-  206/initial_den_A
ratio_den_2018_H<-  1619/initial_den_H
ratio_den_2018_A<-  180/initial_den_A

ratio_den_2020_A<-  155/initial_den_A
ratio_den_2021_A<-  129/initial_den_A
H_A_ratio_den <-initial_den_H/initial_den_A

fittedvalue<-1
LAMBDA_H_temp_input <- fittedvalue

LAMBDA_time <- function(max_time){
  # empty initial vector
  temp_time_H <- rep(NA,max_time)
  temp_time_A <- rep(NA,max_time)
  # run over all time 
  for (i in c(1:max_time) ){
    if (i <= 2)  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio_den 
    } else if ((i >2) & (i <= 3))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A
    } else if ((i >3) & (i <= 15))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A + (i-3)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2015_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A)/(15-3)) 
    } else if ((i >15) & (i <= 16))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A
    }  else if ((i>16) & (i <= 18))   { 
      LAMBDA_H_temp<- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A + (i-16)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A)/(18-16))
    } else if ((i >18) & (i <= 20))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2000+i),]$kg/initial_den_H
      LAMBDA_A_temp <-   (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A + (i-18)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2020_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A)/(20-18)) 
    } else if ((i > 20))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2019),]$kg/initial_den_H 
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2021_A }
    
    #print(c(i,LAMBDA_H_temp, LAMBDA_A_temp))
    
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
  geom_point(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_point(data=as.data.frame(A_data),aes(time,A),col="darkgreen")+
  scale_y_continuous(limits = c(0, 1.01 * max(den_use$kg)))
# geom_line(data=as.data.frame(H_data),aes(time,H),col="darkblue")+geom_line(data=as.data.frame(A_data),aes(time,A),col="darkgreen")#+
ggsave(paste("plots/time_varying_usage_denmark", ".png",sep=""), width = 150, height = 100, units='mm',dpi=1000)

