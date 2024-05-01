###################################################################################
################################    SEFASI transmission model explore fits    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  May 2024 #####################################
###################################################################################

rm(list = ls())
library(tidyverse)
library(data.table)
setwd(here())
theme_set(theme_bw())

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# FULLDATA_DENMARK <-fread("output/OUT_denmark.csv") # old from outfun
# FULLDATA_ENGLAND <-fread("output/OUT_england.csv")
# FULLDATA_SENEGAL <-fread("output/OUT_senegal.csv") 

# new from cluster run 
# FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
# FULLDATA_ENGLAND <-fread("fits/england100000.csv")
# FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

FULLDATA_DENMARK <-fread("fits/denmark4570000.csv")[,-1]
FULLDATA_ENGLAND <-fread("fits/england4570000.csv")[,-1]
FULLDATA_SENEGAL <-fread("fits/senegal4570000.csv")[,-1]

# FULLDATA_DENMARK <-fread("fits/denmark4570.csv")[,-1] 
# FULLDATA_ENGLAND <-fread("fits/england4570.csv")[,-1]
# FULLDATA_SENEGAL <-fread("fits/senegal4570.csv")[,-1]

colnames(FULLDATA_DENMARK) <- c("time", "H", "A", "E", "paraset")
colnames(FULLDATA_ENGLAND) <- c("time", "H", "A", "E", "paraset")
colnames(FULLDATA_SENEGAL) <- c("time", "H", "A", "E", "paraset")

nruns = length(unique(FULLDATA_DENMARK$paraset))

FULLDATA_DENMARK$year <- rep(c(1984,rep(seq(1985,2022,1),each = 12)),nruns)
FULLDATA_ENGLAND$year <- rep(c(1984,rep(seq(1985,2022,1),each = 12)),nruns)
FULLDATA_SENEGAL$year <- rep(c(1984,rep(seq(1985,2022,1),each = 12)),nruns)

maxtime = max(FULLDATA_DENMARK$time)

FULLDATA <- rbind(FULLDATA_DENMARK %>% mutate(ctry = "denmark"),#, runs = seq(1, dim(FULLDATA_DENMARK)[1])), 
                  FULLDATA_ENGLAND %>% mutate(ctry = "england"),#, runs = seq(1, dim(FULLDATA_ENGLAND)[1])), 
                  FULLDATA_SENEGAL %>% mutate(ctry = "senegal")) %>% #, runs = seq(1, dim(FULLDATA_SENEGAL)[1])))
  pivot_longer(cols = c("H","A","E"))
rm(FULLDATA_DENMARK, FULLDATA_ENGLAND, FULLDATA_SENEGAL)

################################ EXPLORE ###########################################
# ggplot(FULLDATA, aes(x=year, y = value, group = runs)) + geom_line() + 
#   facet_grid(ctry ~ env)

ggplot(FULLDATA %>% filter(paraset < 5), aes(x=time, y = value, group = paraset)) + geom_line(aes(col = factor(paraset))) + 
  facet_grid(ctry ~ name)

FULLDATA %>% filter(ctry == "denmark", time == maxtime, name == "H") %>% filter(value < 0.2) %>% summarise(unique(paraset))

ggplot(FULLDATA %>% filter(paraset %in% c(1,14,24,26)), aes(x=year, y = value, group = ctry)) + geom_line(aes(col = factor(ctry))) + facet_grid(paraset ~ name)

ggplot(FULLDATA %>% filter(paraset %in% c(135,218,820)), aes(x=year, y = value, group = ctry)) + geom_line(aes(col = factor(ctry))) + 
  facet_grid(paraset ~ name)

########################################################################################
########################################################################################
########################################################################################
################################  FITTING DATA #####################################
### Resistance prevalence data cleaned 
#source("model_functions/res_prev_band_data.R")

#### Antibiotic usage
#usage.table <- as.data.frame(read.csv("data/usage.csv"))

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
source("0_model_functions.R")

### Explore distance from data for each country 
MLE_SENEGAL <- rep(NA,nruns)
MLE_ENGLAND <- rep(NA,nruns)
MLE_DENMARK <- rep(NA,nruns)

FULLDATA_mean <- FULLDATA %>% group_by(year, ctry, name, paraset) %>%
  summarise(mean_vl = mean(value)) %>%
  ungroup()

## Resistance data to fit to 
res.table <- read_csv("data/res.table.fit.csv")
ggplot(res.table, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))

### How far from data?
data_fit <- res.table %>% select(country,var,time,percent) %>% 
  rename(ctry = country, year = time, name = var) %>%
  mutate(proportion = percent / 100)

joined_fit <- left_join(FULLDATA_mean,data_fit) %>% filter(!is.na(proportion)) %>%
  ungroup() %>%
  mutate(distance = (proportion - mean_vl)^2)
dim(joined_fit) # should be 72(size of data) x nruns

ls_summ <- joined_fit %>% group_by(ctry, paraset) %>%
  summarise(dist = sum(distance))

### Store
write.csv(ls_summ, "output/LS_values.csv")


## This is the model output ##
# Find the max 100 and store
ls_top <- ls_summ %>% 
  arrange(dist) %>% 
  group_by(ctry) %>% slice(1:100) %>%
  ungroup()

ls_top_sen <- ls_top %>% filter(ctry == "senegal") %>% select(paraset) %>% pull()
ls_top_eng <- ls_top %>% filter(ctry == "senegal") %>% select(paraset) %>% pull()
ls_top_den <- ls_top %>% filter(ctry == "senegal") %>% select(paraset) %>% pull()
best_100_senegal <- FULLDATA %>% filter(paraset %in% ls_top_sen)
best_100_england <- FULLDATA %>% filter(paraset %in% ls_top_eng)
best_100_denmark <- FULLDATA %>% filter(paraset %in% ls_top_den)

write.csv(best_100_senegal,"output/best_100_senegal.csv")
write.csv(best_100_england,"output/best_100_england.csv")
write.csv(best_100_denmark,"output/best_100_denmark.csv")

# 
# ####### Find the max 100 and store
# eng <- cbind(FULLDATA_ENGLAND, MLE_ENGLAND)
# data_eng21 = res.table %>% filter(country == "england", var == "H", time == 2021)
# b <- as.vector(best_100_england[1:10,1])$V1
# ggplot(eng, aes(x=MLE_ENGLAND, y = model2021.H)) + geom_point() + 
#   geom_hline(yintercept = data_eng21$percent/100, col = "red") + 
#   geom_point(data = eng %>% filter(V1 %in% b), col = "blue", pch = 10)
# 
# eng %>% filter(MLE_ENGLAND < 0.01) %>% select(model2021.H, MLE_ENGLAND)
# eng %>% filter(MLE_ENGLAND == min(eng$MLE_ENGLAND)) %>% select(model2021.H, MLE_ENGLAND, V1)

## The best 100 parameter sets are: 
p <- read.csv("output/parameter_set_100000.csv")[,-1] %>% as.data.frame()
colnames(p) <- c("LAMBDA_H","LAMBDA_A","LAMBDA_E","beta_HH","beta_AA","beta_EE","beta_AH",
  "beta_HA","beta_EH","beta_EA","beta_AE","beta_HE","mu_H","mu_A","mu_E","para")
best_100_para_senegal <- p %>% filter(para %in% ls_top_sen)
best_100_para_england <- p %>% filter(para %in% ls_top_eng)
best_100_para_denmark <- p %>% filter(para %in% ls_top_den)

write.csv(best_100_para_senegal,"output/best_100_para_senegal.csv")
write.csv(best_100_para_england,"output/best_100_para_england.csv")
write.csv(best_100_para_denmark,"output/best_100_para_denmark.csv")


########################################################################################
########################################################################################
########################################################################################
############################################ Visualise fits ############################################
#### Read in what need
res.table.fit <- read_csv("data/res.table.fit.csv") %>% mutate(prop = percent / 100)

best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1]
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1]
best_100_england <- read_csv("output/best_100_england.csv")[,-1]

best <- rbind(best_100_senegal %>% mutate(country = "senegal"),
              best_100_england %>% mutate(country ="england"),
              best_100_denmark %>% mutate(country ="denmark"))
# rename
c <- colnames(best)
c[1] <- "run"
c[68] <- "country"
colnames(best) <- c

# pivot around
model_output <- best %>% 
  pivot_longer("model2000.H":"model2021.E") %>% 
  mutate(environment = sub(".*\\.", "", name), 
         year = as.numeric(substring(str_extract(name, "^[^\\.:]+"), nchar(str_extract(name, "^[^\\.:]+"))-3)))

data_fit <- res.table.fit %>% mutate(environment = var)

ggplot(model_output, aes(x=year, y = value, group = run)) + geom_line() + 
  geom_point(data = data_fit,aes(x=time, y = prop, group = country), col = "red") + 
  facet_wrap(country~environment,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits.pdf")

ggplot(model_output, aes(x=year, y = value, group = run)) + geom_line() + 
  geom_point(data = data_fit,aes(x=time, y = prop, group = country), col = "red") + 
  facet_wrap(environment~country,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_flip.pdf")

############# Visualise parameter outputs
p1 <- read.csv("output/parameter_set_100000.csv")

runs <- best$run
length(unique(runs)) # not the same for the countries => 300 unique 

write_csv(cbind(p1[runs,], "country" = best$country), "output/best_parameter_sets.csv")

#### Visualise parameters
fit_para <- cbind(p1[runs,], best$country) %>% pivot_longer(cols = LAMBDA_H:epsilon)
colnames(fit_para) <- c("pset","country","para","value")
ggplot(fit_para, aes(x=para, y = value)) + geom_violin() + facet_wrap(~country, ncol = 1)
ggplot(fit_para, aes(x=para, y = value)) + geom_boxplot() + facet_wrap(~country, ncol = 1)



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
>>>>>>> b644649050f2a34241d46fa6d31f48ffea7324c5

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
