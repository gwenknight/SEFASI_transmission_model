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
MLE_SENEGAL <- rep(NA,nruns)
MLE_ENGLAND <- rep(NA,nruns)
MLE_DENMARK <- rep(NA,nruns)

FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

#### Data with one data point per time point
res.table <- read_csv("data/res.table.fit.csv")
ggplot(res.table, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))

### See how far from data 
for(ii in 1:nruns) {
  MLE_SENEGAL[ii] =  LS_simple(FULLDATA_SENEGAL[ii,], "senegal", res.table)
  print(ii)
}

for(ii in 1:nruns) {
  MLE_DENMARK[ii] =  LS_simple(FULLDATA_DENMARK[ii,], "denmark", res.table)
  print(ii)
}

for(ii in 1:nruns) {
  MLE_ENGLAND[ii] =  LS_simple(FULLDATA_ENGLAND[ii,],"england", res.table)
  print(ii)
}

# Store log-likelihood values 
write.csv(MLE_SENEGAL,"output/MLE_SENEGAL.csv")
write.csv(MLE_DENMARK,"output/MLE_DENMARK.csv")
write.csv(MLE_ENGLAND,"output/MLE_ENGLAND.csv")

MLE_SENEGAL <- read.csv("output/MLE_SENEGAL.csv")[,-1]
MLE_DENMARK <- read.csv("output/MLE_DENMARK.csv")[,-1]
MLE_ENGLAND <- read.csv("output/MLE_ENGLAND.csv")[,-1]

####### Find the max 100 and store
eng <- cbind(FULLDATA_ENGLAND, MLE_ENGLAND)
data_eng21 = res.table %>% filter(country == "england", var == "H", time == 2021)
b <- as.vector(best_100_england[1:10,1])$V1
ggplot(eng, aes(x=MLE_ENGLAND, y = model2021.H)) + geom_point() + 
  geom_hline(yintercept = data_eng21$percent/100, col = "red") + 
  geom_point(data = eng %>% filter(V1 %in% b), col = "blue", pch = 10)

eng %>% filter(MLE_ENGLAND < 0.01) %>% select(model2021.H, MLE_ENGLAND)
eng %>% filter(MLE_ENGLAND == min(eng$MLE_ENGLAND)) %>% select(model2021.H, MLE_ENGLAND, V1)


## This is the model output
best_100_senegal <- FULLDATA_SENEGAL[order(MLE_SENEGAL)[1:100],]
best_100_england <- FULLDATA_ENGLAND[order(MLE_ENGLAND)[1:100],]
best_100_denmark <- FULLDATA_DENMARK[order(MLE_DENMARK)[1:100],]
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
p <- read.csv("output/parameter_set_100000.csv")
FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

MLE_SENEGAL <- read.csv("output/MLE_SENEGAL.csv")[,-1]
MLE_DENMARK <- read.csv("output/MLE_DENMARK.csv")[,-1]
MLE_ENGLAND <- read.csv("output/MLE_ENGLAND.csv")[,-1]

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
source("model_functions/AMRmodel.R")
source("plot_functions/explore_and_plot_time_varying_usage.R")
source("model_functions/epid.R")

source("plot_functions/plotfits2.R") # GK? use ENGLAND as example

################################  plotfits function, in order to plot the fits #####################################
#plotfits2(FITS_DENMARK,"denmark",0.025) 
#plotfits2(FITS_ENGLAND,"england",0.035) 
#plotfits2(FITS_SENEGAL,"senegal",0.01) 

plotfits2(best_100_para_senegal,"senegal",0) 
plotfits2(best_100_para_denmark,"denmark",0) 
plotfits2(best_100_para_england,"england",0) 

################### Visualise fits 
best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1] %>% mutate(country = "senegal")
best_100_england <- read_csv("output/best_100_england.csv")[,-1] %>% mutate(country = "england")
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1] %>% mutate(country = "denmark")

fits <- rbind(best_100_denmark, best_100_england, best_100_senegal)
colnames(fits) <- c("para",colnames(fits)[-1])
length(unique(fits$para)) # 220 unique ones -> some overlap across countries? 

fits_long <- fits %>% pivot_longer(cols = model2000.H:model2021.E) %>% 
  rowwise() %>% 
  mutate(env = sub('.*\\.', '', name),
         yeara = sub('.*model', '', name),
         year = strsplit(yeara,"\\.")[[1]][1]) %>% 
  select(-c(yeara, name))

fits_long$year <- as.numeric(fits_long$year)

res.table$env <- res.table$var

ggplot(fits_long, aes(x=year, y = value, group = para)) + 
  geom_line() + 
  facet_wrap(country ~ env) + 
  geom_point(data = res.table, aes(x=time, y = percent/100, group = env, col = env))

ggplot(fits_long, aes(x=year, y = value, group = year)) + 
  geom_boxplot() + 
  facet_wrap(country ~ env) + 
  geom_point(data = res.table, aes(x=time, y = percent/100, group = env, col = env))
ggsave("plots/fits_boxplot.pdf")

### Averages
fits_av <- fits_long %>% group_by(year, country, env) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975)))

ggplot(fits_av, aes(x=year, y = mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = min025, ymax = max975)) + 
  facet_wrap(country ~ env) + 
  geom_point(data = res.table, aes(x=time, y = percent/100, group = env, col = env))
ggsave("plots/fits_mean_quantile.pdf")
