###################################################################################
################################    SEFASI transmission model explore output    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
FULLDATA_DENMARK <-fread("output/OUT_denmark.csv") 
FULLDATA_ENGLAND <-fread("output/OUT_england.csv") 
FULLDATA_SENEGAL <-fread("output/OUT_senegal.csv") 


################################  FITTING DATA #####################################
### Resistance prevalence data cleaned 
source("model_functions/res_prev_band_data.R")

#### Antibiotic usage
usage.table <- as.data.frame(read.csv("data/usage.csv"))

#### Plot this input data - only do first time
# source("plotinputdata.R")
# #### How far can the model be from the data? 
# MARGIN_TOLERANCE = 0.10
# # Adapts the resistance prevalence data 
# source("model_functions/total_max_min_example.R") # For each year, take the max / min of the data for resistance prevalence +/- margin of tolerance. Ignore warnings() - to do with years that have no data
# # -> Generates "sup_inf_data"

# #source("how.many.fit.R")
# 
# #FITS_DENMARK <- how.many.fit(FULLDATA_orig,"denmark",0.025)
# #nrow(FITS_DENMARK)
# #FITS_ENGLAND <- how.many.fit(FULLDATA_orig,"england",0.035)
# #nrow(FITS_ENGLAND)
# #FITS_SENEGAL <- how.many.fit(FULLDATA_orig,"senegal",0.13)
# #nrow(FITS_SENEGAL)
# 
# #FULLDATA <- FITS_DENMARK

## Load function to calculate log likelihood 
source("model_functions/LL_function.R")

### Explore distance from data for each country 
MLE_SENEGAL <- rep(NA,nrow(FULLDATA_SENEGAL) )
MLE_ENGLAND <- rep(NA,nrow(FULLDATA_ENGLAND) )
MLE_DENMARK <- rep(NA,nrow(FULLDATA_DENMARK) )

for(i in 1:nrow(FULLDATA_SENEGAL)) {
  MLE_SENEGAL[i] =  LL_simple(FULLDATA_SENEGAL[i,], "senegal", res.table)
  #print(i)
}

for(i in 1:nrow(FULLDATA_DENMARK)) {
  MLE_DENMARK[i] =  LL_simple(FULLDATA_DENMARK[i,], "denmark", res.table)
 # print(i)
}

for(i in 1:nrow(FULLDATA_ENGLAND)) {
  MLE_ENGLAND[i] =  LL_simple(FULLDATA_ENGLAND[i,],"england", res.table)
#  print(i)
}

# Store log-likelihood values 
write.csv(MLE_SENEGAL,"output/MLE_SENEGAL.csv")
write.csv(MLE_DENMARK,"output/MLE_DENMARK.csv")
write.csv(MLE_ENGLAND,"output/MLE_ENGLAND.csv")

# Find the max 100 and store
best_100_senegal <- FULLDATA_SENEGAL[order(-MLE_SENEGAL)[1:100],]
best_100_england <- FULLDATA_ENGLAND[order(-MLE_ENGLAND)[1:100],]
best_100_denmark <- FULLDATA_DENMARK[order(-MLE_DENMARK)[1:100],]


write.csv(best_100_senegal,"output/best_100_senegal.csv")
write.csv(best_100_england,"output/best_100_england.csv")
write.csv(best_100_denmark,"output/best_100_denmark.csv")



################################ boxplot of parameters which are selected #####################################
source("plot_functions/boxplots.R") # GK? use ENGLAND as example

################################  plotfits function, in order to plot the fits #####################################
#plotfits2(FITS_DENMARK,"denmark",0.025) 
#plotfits2(FITS_ENGLAND,"england",0.035) 
#plotfits2(FITS_SENEGAL,"senegal",0.01) 

plotfits2(best_100_senegal,"senegal",0) 
plotfits2(best_100_denmark,"denmark",0) 
plotfits2(best_100_england,"england",0) 

### Load functions to run interventions 
source("model_functions/AMRmodel.R")
source("model_functions/epid.R")
source("plot_functions/explore_and_plot_time_varying_usage.R")
source("model_functions/epid_intervention.R")
source("plot_functions/plotfits_int.R")

sup_inf_data <- read.csv("output/sup_inf_data")
plotfits_int(best_100_england,"england",0, sup_inf_data)


################################ boxplot of interventions  ################################ 
getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white","black")

source("plotfits_int_box.R")

plotfits_int_box(best_100_england,"england")
plotfits_int_box(best_100_denmark,"denmark")
plotfits_int_box(best_100_senegal,"senegal")

source("combined_plots.R") #produces the plots by interventiona and country



