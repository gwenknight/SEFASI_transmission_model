###################################################################################
################################    SEFASI transmission model    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())

############################## Input parameters 
input.table <- as.data.frame(read.csv("data/input.table.csv"))
input.table$parameter <- as.character(input.table$parameter)

################################  FITTING DATA #####################################
#### Resistance prevalence
res.table<- as.data.frame(read.csv("data/res.table.csv"))
res.table$percent <- res.table$percent/100
res.table$inf <- res.table$percent - (1.96*sqrt(res.table$percent*(1-res.table$percent)/res.table$N))
res.table$sup <- res.table$percent + (1.96*sqrt(res.table$percent*(1-res.table$percent)/res.table$N))
res.table$inf[res.table$inf<0] <- 0 #make sure no negatives
res.table<-res.table[res.table$country %in% c("denmark","england","senegal"),] # only data for the countries in SEFASI



#### Plot this input data - only do first time
# source("plotinputdata.R")

#### How far can the model be from the data? 
MARGIN_TOLERANCE = 0.10
# Adapts the resistance prevalence data 
source("model_functions/total_max_min_example.R") # For each year, take the max / min of the data for resistance prevalence +/- margin of tolerance. Ignore warnings() - to do with years that have no data
# -> Generates "sup_inf_data"

#### Load needed functions
# Loads Core AMR model
source("model_functions/AMRmodel.R")
# Loads epid function: wrapper for core model 
source("model_functions/epid.R")
# Loads Sampling function: LHS sampling with checks on parameters
source("model_functions/sampling.R")
# Loads function to run across LHS parameters (outFun and temp one for non parallel)
source("model_functions/run_for_many_para.R")
# Plot output functions
source("plot_functions/plotfits.R")
source("plot_functions/plotfits2.R")

##### Generate parameter samples
p1<-sampling(500000) # go big to get lots as criterion remove lots
## Add final limitations
p1 <- p1[p1$beta_EA >= p1$beta_EH, ] # removes a lot
## Take 100,000 of these: do random as LHS aims to go over the whole space
p1 <- p1[sample(x = dim(p1)[1], size = 100000),]
dim(p1) # 100,000 parameter sets

##### Generate time varying antibiotic usage curves
# will plot england and denmark assumptions for usage & generates time varying LAMBDA
source("plot_functions/explore&plot_time_varying_usage.R") 


#Run the simulator outFUN for the Latin-Hypercube samples generated above
# Not parallel
#ptm <- proc.time() #time run 
#outFUN_temp(p1[1:100,],"denmark" #the file name
#) 
#proc.time() - ptm

### GK: computer 0.5s per run
ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"england" #the file name 
) 
proc.time() - ptm

ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"denmark" #the file name
) 
proc.time() - ptm

ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"senegal" #the file name
) 
proc.time() - ptm



#FULLDATA fitting read in 
FULLDATA_orig_DENMARK <-fread("OUT_denmark.csv")
FULLDATA_DENMARK <-fread("OUT_denmark.csv") 

FULLDATA_orig_ENGLAND <-fread("OUT_england.csv")
FULLDATA_ENGLAND <-fread("OUT_england.csv") 

FULLDATA_orig_SENEGAL <-fread("OUT_senegal.csv")
FULLDATA_SENEGAL <-fread("OUT_senegal.csv") 
#source("how.many.fit.R")

#FITS_DENMARK <- how.many.fit(FULLDATA_orig,"denmark",0.025)
#nrow(FITS_DENMARK)
#FITS_ENGLAND <- how.many.fit(FULLDATA_orig,"england",0.035)
#nrow(FITS_ENGLAND)
#FITS_SENEGAL <- how.many.fit(FULLDATA_orig,"senegal",0.13)
#nrow(FITS_SENEGAL)

#FULLDATA <- FITS_DENMARK
 

source("LL_function.R")
source("LL_simple_senegal.R")
source("LL_simple_england.R")
source("LL_simple_denmark.R")

MLE_SENEGAL <- rep(NA,nrow(FULLDATA_orig_SENEGAL) )
MLE_ENGLAND <- rep(NA,nrow(FULLDATA_orig_ENGLAND) )
MLE_DENMARK <- rep(NA,nrow(FULLDATA_orig_DENMARK) )

for(i in 1:nrow(FULLDATA_orig_SENEGAL)) {
  MLE_SENEGAL[i] =  LL_simple_senegal(FULLDATA_orig_SENEGAL[i,])
  print(i)
}

for(i in 1:nrow(FULLDATA_orig_DENMARK)) {
  MLE_DENMARK[i] =  LL_simple_denmark(FULLDATA_orig_DENMARK[i,])
  print(i)
}

for(i in 1:nrow(FULLDATA_orig_ENGLAND)) {
  MLE_ENGLAND[i] =  LL_simple_england(FULLDATA_orig_ENGLAND[i,])
  print(i)
}

write.csv(MLE_SENEGAL,"MLE_SENEGAL.csv")
write.csv(MLE_DENMARK,"MLE_DENMARK.csv")
write.csv(MLE_ENGLAND,"MLE_ENGLAND.csv")


#order(-MLE_SENEGAL)[1:100]
best_100_senegal <- FULLDATA_orig_SENEGAL[order(-MLE_SENEGAL)[1:100],]
best_100_england <- FULLDATA_orig_ENGLAND[order(-MLE_ENGLAND)[1:100],]
best_100_denmark <- FULLDATA_orig_DENMARK[order(-MLE_DENMARK)[1:100],]


write.csv(best_100_senegal,"best_100_senegal.csv")
write.csv(best_100_england,"best_100_england.csv")
write.csv(best_100_denmark,"best_100_denmark.csv")




################################ boxplot of parameters which are selected #####################################
source("boxplots.R") #use ENGLAND as example

################################  plotfits function, in order to plot the fits #####################################
#plotfits2(FITS_DENMARK,"denmark",0.025) 
#plotfits2(FITS_ENGLAND,"england",0.035) 
#plotfits2(FITS_SENEGAL,"senegal",0.01) 

plotfits2(best_100_senegal,"senegal",0) 
plotfits2(best_100_denmark,"denmark",0) 
plotfits2(best_100_england,"england",0) 

source("epid_intervention.R")
source("plotfits_int.R")

#plotfits_int(best_100_senegal,"senegal",0)


################################ boxplot of interventions  ################################ 
getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white","black")

source("plotfits_int_box.R")

plotfits_int_box(best_100_england,"england")
plotfits_int_box(best_100_denmark,"denmark")
plotfits_int_box(best_100_senegal,"senegal")

source("combined_plots.R") #produces the plots by interventiona and country




