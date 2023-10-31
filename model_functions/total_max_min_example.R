#### Modify the resistance prevalence data to create the band for the model to fall within 
# For each year from 2000 - 2021 take the highest and lowest level of resistance found in the data
# Will get warnings() if data doesn't exist for that year but code will still run
# Could extract the values from the big table
# INPUTS res.table
# OUTPUTS sup_inf_data

### For Denmark data
denmark_res <- res.table[res.table$country=="denmark",]

for(i in 1:21){
  #nam_max_H <- paste("den_res_H_",2000+i,"_max",sep="")
  assign( paste("den_res_H_",2000+i,"_max",sep=""),
         max(denmark_res[denmark_res$var=="H" & denmark_res$time == 2000+i,]$percent))
 # nam_min_H <- paste("den_res_H_",2000+i,"_min",sep="")
  assign(paste("den_res_H_",2000+i,"_min",sep=""),
         min(denmark_res[denmark_res$var=="H" & denmark_res$time == 2000+i,]$percent))
  
#  nam_max_A <- paste("den_res_A_",2000+i,"_max",sep="")
 assign(paste("den_res_A_",2000+i,"_max",sep=""),
        max(denmark_res[denmark_res$var=="A" & denmark_res$time == 2000+i,]$percent))
 #nam_min_A <- paste("den_res_A_",2000+i,"_min",sep="")
 assign(paste("den_res_A_",2000+i,"_min",sep=""),
        min(denmark_res[denmark_res$var=="A" & denmark_res$time == 2000+i,]$percent))
 
 #nam_max_E <- paste("den_res_E_",2000+i,"_max",sep="")
 assign(paste("den_res_E_",2000+i,"_max",sep=""),
        max(denmark_res[denmark_res$var=="E" & denmark_res$time == 2000+i,]$percent))
 #nam_min_E <- paste("den_res_E_",2000+i,"_min",sep="")
 assign(paste("den_res_E_",2000+i,"_min",sep=""),
        min(denmark_res[denmark_res$var=="E" & denmark_res$time == 2000+i,]$percent))
}

den_res_A <- denmark_res[1:21,]
den_res_A$var <- "A"
den_res_A$time <- 2001:2021
den_res_A[,4:16] <- NA
den_res_H <- den_res_A
den_res_H$var <- "H"
den_res_E <- den_res_A
den_res_E$var <- "E"

den_res_H$inf <- sapply(paste("den_res_H_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
den_res_H$sup <- sapply(paste("den_res_H_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
den_res_A$inf <- sapply(paste("den_res_A_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
den_res_A$sup <- sapply(paste("den_res_A_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
den_res_E$inf <- sapply(paste("den_res_E_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
den_res_E$sup <- sapply(paste("den_res_E_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))

den_res <- rbind(den_res_H,den_res_A,den_res_E)
den_res$sup[!is.finite(den_res$sup)] <- NA
den_res$inf[!is.finite(den_res$inf)] <- NA

### For English data
england_res <- res.table[res.table$country=="england",]

for(i in 1:21){
  #nam_max_H <- paste("eng_res_H_",2000+i,"_max",sep="")
  assign(paste("eng_res_H_",2000+i,"_max",sep=""),
         max(england_res[england_res$var=="H" & england_res$time == 2000+i,]$percent))
 # nam_min_H <- paste("eng_res_H_",2000+i,"_min",sep="")
  assign(paste("eng_res_H_",2000+i,"_min",sep=""),
         min(england_res[england_res$var=="H" & england_res$time == 2000+i,]$percent))
  
 # nam_max_A <- paste("eng_res_A_",2000+i,"_max",sep="")
  assign(paste("eng_res_A_",2000+i,"_max",sep=""),
         max(england_res[england_res$var=="A" & england_res$time == 2000+i,]$percent))
  #nam_min_A <- paste("eng_res_A_",2000+i,"_min",sep="")
  assign(paste("eng_res_A_",2000+i,"_min",sep=""),
         min(england_res[england_res$var=="A" & england_res$time == 2000+i,]$percent))
  
 # nam_max_E <- paste("eng_res_E_",2000+i,"_max",sep="")
  assign(paste("eng_res_E_",2000+i,"_max",sep=""),
         max(england_res[england_res$var=="E" & england_res$time == 2000+i,]$percent))
  #nam_min_E <- paste("eng_res_E_",2000+i,"_min",sep="")
  assign(paste("eng_res_E_",2000+i,"_min",sep=""),
         min(england_res[england_res$var=="E" & england_res$time == 2000+i,]$percent))
}

eng_res_A <- england_res[1:21,]
eng_res_A$var <- "A"
eng_res_A$time <- 2001:2021
eng_res_A[,4:16] <- NA
eng_res_H <- eng_res_A
eng_res_H$var <- "H"
eng_res_E <- eng_res_A
eng_res_E$var <- "E"

eng_res_H$inf <- sapply(paste("eng_res_H_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
eng_res_H$sup <- sapply(paste("eng_res_H_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
eng_res_A$inf <- sapply(paste("eng_res_A_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
eng_res_A$sup <- sapply(paste("eng_res_A_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
eng_res_E$inf <- sapply(paste("eng_res_E_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
eng_res_E$sup <- sapply(paste("eng_res_E_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))

eng_res <- rbind(eng_res_H,eng_res_A,eng_res_E)
eng_res$sup[!is.finite(eng_res$sup)] <- NA
eng_res$inf[!is.finite(eng_res$inf)] <- NA


### For Senegal data
senegal_res <- res.table[res.table$country=="senegal",]

for(i in 1:21){
 # nam_max_H <- paste("sen_res_H_",2000+i,"_max",sep="")
  assign(paste("sen_res_H_",2000+i,"_max",sep=""),
         max(senegal_res[senegal_res$var=="H" & senegal_res$time == 2000+i,]$percent))
  #nam_min_H <- paste("sen_res_H_",2000+i,"_min",sep="")
  assign(paste("sen_res_H_",2000+i,"_min",sep=""),
         min(senegal_res[senegal_res$var=="H" & senegal_res$time == 2000+i,]$percent))
  
 # nam_max_A <- paste("sen_res_A_",2000+i,"_max",sep="")
  assign(paste("sen_res_A_",2000+i,"_max",sep=""),
         max(senegal_res[senegal_res$var=="A" & senegal_res$time == 2000+i,]$percent))
 # nam_min_A <- paste("sen_res_A_",2000+i,"_min",sep="")
  assign(paste("sen_res_A_",2000+i,"_min",sep=""),
         min(senegal_res[senegal_res$var=="A" & senegal_res$time == 2000+i,]$percent))
  
#  nam_max_E <- paste("sen_res_E_",2000+i,"_max",sep="")
  assign(paste("sen_res_E_",2000+i,"_max",sep=""),
         max(senegal_res[senegal_res$var=="E" & senegal_res$time == 2000+i,]$percent))
 # nam_min_E <- paste("sen_res_E_",2000+i,"_min",sep="")
  assign(paste("sen_res_E_",2000+i,"_min",sep=""),
         min(senegal_res[senegal_res$var=="E" & senegal_res$time == 2000+i,]$percent))
}

sen_res_A <- england_res[1:21,] #set as england temp in this case as <21 data points in senegal
sen_res_A$country <- "senegal"
sen_res_A$var <- "A"
sen_res_A$time <- 2001:2021
sen_res_A[,4:16] <- NA
sen_res_H <- sen_res_A
sen_res_H$var <- "H"
sen_res_E <- sen_res_A
sen_res_E$var <- "E"

sen_res_H$inf <- sapply(paste("sen_res_H_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
sen_res_H$sup <- sapply(paste("sen_res_H_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
sen_res_A$inf <- sapply(paste("sen_res_A_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
sen_res_A$sup <- sapply(paste("sen_res_A_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))
sen_res_E$inf <- sapply(paste("sen_res_E_",2000+1:21,"_min",sep=""), function(x) eval(parse(text=x)))
sen_res_E$sup <- sapply(paste("sen_res_E_",2000+1:21,"_max",sep=""), function(x) eval(parse(text=x)))

sen_res <- rbind(sen_res_H,sen_res_A,sen_res_E)
sen_res$sup[!is.finite(sen_res$sup)] <- NA
sen_res$inf[!is.finite(sen_res$inf)] <- NA


#### Combine together into one dataset and add margin of tolerance 
sup_inf_data <- rbind(eng_res,den_res,sen_res)

write.csv(sup_inf_data, "output/sup_inf_data.csv")


# sup_inf_data[!is.na(sup_inf_data$inf),]$inf <- sup_inf_data[!is.na(sup_inf_data$inf),]$inf  - MARGIN_TOLERANCE
# sup_inf_data[sup_inf_data$inf<=0 & !is.na(sup_inf_data$inf),]$inf <- 0
# sup_inf_data[!is.na(sup_inf_data$sup),]$sup <- sup_inf_data[!is.na(sup_inf_data$sup),]$sup  + MARGIN_TOLERANCE
# sup_inf_data<-sup_inf_data[!is.na(sup_inf_data$sup),]



