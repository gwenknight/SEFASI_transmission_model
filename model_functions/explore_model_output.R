###################################################################################
################################    SEFASI transmission model explore fits    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())
theme_set(theme_bw())

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# FULLDATA_DENMARK <-fread("output/OUT_denmark.csv") # old from outfun
# FULLDATA_ENGLAND <-fread("output/OUT_england.csv")
# FULLDATA_SENEGAL <-fread("output/OUT_senegal.csv") 

# new from cluster run 
FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

nruns = nrow(FULLDATA_DENMARK)

FULLDATA <- rbind(FULLDATA_DENMARK %>% mutate(ctry = "denmark", runs = seq(1, dim(FULLDATA_DENMARK)[1])), 
                  FULLDATA_ENGLAND %>% mutate(ctry = "england", runs = seq(1, dim(FULLDATA_ENGLAND)[1])), 
                  FULLDATA_SENEGAL %>% mutate(ctry = "senegal", runs = seq(1, dim(FULLDATA_SENEGAL)[1])))
rm(FULLDATA_DENMARK, FULLDATA_ENGLAND, FULLDATA_SENEGAL)

################################ EXPLORE ###########################################
FULLDATA2 <- FULLDATA %>% pivot_longer(cols = model2000.H:model2021.E) 
FULLDATA2$year2 <- sub('.*model', '', FULLDATA2$name)
FULLDATA2$year <- str_split_fixed(FULLDATA2$year2, fixed("."), 2)[, 1]
FULLDATA2$env <- str_split_fixed(FULLDATA2$year2, fixed("."), 2)[, 2]
FULLDATA <- FULLDATA2 %>% select(-c(year2, name))

# ggplot(FULLDATA, aes(x=year, y = value, group = runs)) + geom_line() + 
#   facet_grid(ctry ~ env)

ggplot(FULLDATA %>% filter(runs < 5), aes(x=year, y = value, group = runs)) + geom_line(aes(col = factor(runs))) + 
  facet_grid(ctry ~ env)

FULLDATA %>% filter(ctry == "denmark", year == 2021, env == "H") %>% filter(value < 0.2)

ggplot(FULLDATA %>% filter(runs %in% c(135,218,820)), aes(x=year, y = value, group = ctry)) + geom_line(aes(col = factor(ctry))) + 
  facet_grid(runs ~ env)

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
MLE_SENEGAL <- rep(NA,nruns )
MLE_ENGLAND <- rep(NA,nruns )
MLE_DENMARK <- rep(NA,nruns )

#### Data with one data point per time point
res.table <- read_csv("data/res.table.fit.csv")
ggplot(res.table, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))

### See how far from data 
for(i in 1:nruns) {
  MLE_SENEGAL[i] =  LL_simple(FULLDATA_SENEGAL[i,], "senegal", res.table)
  print(i)
}

for(i in 1:nruns) {
  MLE_DENMARK[i] =  LL_simple(FULLDATA_DENMARK[i,], "denmark", res.table)
  print(i)
}

for(i in 1:nruns) {
  MLE_ENGLAND[i] =  LL_simple(FULLDATA_ENGLAND[i,],"england", res.table)
  print(i)
}

# Store log-likelihood values 
write.csv(MLE_SENEGAL,"output/MLE_SENEGAL.csv")
write.csv(MLE_DENMARK,"output/MLE_DENMARK.csv")
write.csv(MLE_ENGLAND,"output/MLE_ENGLAND.csv")

FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

####### Find the max 100 and store
## This is the model output
best_100_senegal <- FULLDATA_SENEGAL[order(-MLE_SENEGAL)[1:100],]
best_100_england <- FULLDATA_ENGLAND[order(-MLE_ENGLAND)[1:100],]
best_100_denmark <- FULLDATA_DENMARK[order(-MLE_DENMARK)[1:100],]
## The parameter sets are: 
p <- read.csv("output/parameter_set_100000.csv")
best_100_para_senegal <- p[order(-MLE_SENEGAL)[1:100],]
best_100_para_england <- p[order(-MLE_ENGLAND)[1:100],]
best_100_para_denmark <- p[order(-MLE_DENMARK)[1:100],]

write.csv(best_100_senegal,"output/best_100_senegal.csv")
write.csv(best_100_england,"output/best_100_england.csv")
write.csv(best_100_denmark,"output/best_100_denmark.csv")

write.csv(best_100_para_senegal,"output/best_100_para_senegal.csv")
write.csv(best_100_para_england,"output/best_100_para_england.csv")
write.csv(best_100_para_denmark,"output/best_100_para_denmark.csv")


#####################################################################################
#####################################################################################
##### Read in if doing later analysis
#####################################################################################
FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

MLE_SENEGAL <- read.csv("output/MLE_SENEGAL.csv")
MLE_DENMARK <- read.csv("output/MLE_DENMARK.csv")
MLE_ENGLAND <- read.csv("output/MLE_ENGLAND.csv")

best_100_senegal <- read.csv("output/best_100_senegal.csv")
best_100_england <- read.csv("output/best_100_england.csv")
best_100_denmark <- read.csv("output/best_100_denmark.csv")

best_100_para_senegal <- read.csv("output/best_100_para_senegal.csv")
best_100_para_england <- read.csv("output/best_100_para_england.csv")
best_100_para_denmark <- read.csv("output/best_100_para_denmark.csv")

############## Visualise data 
ggplot(res.table, aes(x = time, y = percent, group = interaction(var, country))) + geom_line(aes(col= country)) + facet_wrap(~var, ncol = 3)
# res_H_country <- res.table[res.table$country==input_country & res.table$var=="H",]
# res_A_country <- res.table[res.table$country==input_country & res.table$var=="A",]
# res_E_country <- res.table[res.table$country==input_country & res.table$var=="E",]

################################ boxplot of parameters which are selected #####################################
## Generates boxplot image of original inputted parameters and those selected for each country
source("plot_functions/boxplots.R")

source("plot_functions/plotfits2.R") # GK? use ENGLAND as example

################################  plotfits function, in order to plot the fits #####################################
#plotfits2(FITS_DENMARK,"denmark",0.025) 
#plotfits2(FITS_ENGLAND,"england",0.035) 
#plotfits2(FITS_SENEGAL,"senegal",0.01) 

plotfits2(best_100_senegal,"senegal",0) 
plotfits2(best_100_out_denmark,"denmark",0) 
plotfits2(best_100_out_england,"england",0) 

