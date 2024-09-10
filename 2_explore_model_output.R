#################################################################################
################################  SEFASI transmission model explore fits    #####
######### Gwen Knight based on initial work by Ross Booton ####################
########## May 2024 ###########################################################

#### ORIGINAL TO PLAY WITH SMALL SAMPLES 
#### Use _big.R if too large to load all together

rm(list = ls())
library(tidyverse)
library(data.table)
setwd(here())
theme_set(theme_bw())

# initial values
source("0_initial_conditions.R")



################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# FULLDATA_DENMARK <-fread("output/OUT_denmark.csv") # old from outfun
# FULLDATA_ENGLAND <-fread("output/OUT_england.csv")
# FULLDATA_SENEGAL <-fread("output/OUT_senegal.csv") 

# new from cluster run 
# FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
# FULLDATA_ENGLAND <-fread("fits/england100000.csv")
# FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")

# FULLDATA_DENMARK <-fread("fits/denmark45700000.csv")[,-1]
# FULLDATA_ENGLAND <-fread("fits/england45700000.csv")[,-1]
# FULLDATA_SENEGAL <-fread("fits/senegal45700000.csv")[,-1]

FULLDATA_DENMARK <-fread("fits/denmark2050.csv")[,-1]
FULLDATA_ENGLAND <-fread("fits/england2530.csv")[,-1]
FULLDATA_SENEGAL <-fread("fits/senegal2290.csv")[,-1]

colnames(FULLDATA_DENMARK) <- c("time", "H", "A", "E", "paraset")
colnames(FULLDATA_ENGLAND) <- c("time", "H", "A", "E", "paraset")
colnames(FULLDATA_SENEGAL) <- c("time", "H", "A", "E", "paraset")

nruns = length(unique(FULLDATA_DENMARK$paraset))

FULLDATA_DENMARK$year <- rep(c(init_denmark_year,rep(seq(init_denmark_year+1,2022,1),each = 12)),nruns)
FULLDATA_ENGLAND$year <- rep(c(init_england_year,rep(seq(init_england_year+1,2022,1),each = 12)),nruns)
FULLDATA_SENEGAL$year <- rep(c(init_senegal_year,rep(seq(init_senegal_year+1,2022,1),each = 12)),nruns)

FULLDATA_DENMARK <- FULLDATA_DENMARK %>% pivot_longer(cols = c("H","A","E"))
FULLDATA_ENGLAND <- FULLDATA_ENGLAND %>% pivot_longer(cols = c("H","A","E"))
FULLDATA_SENEGAL <- FULLDATA_SENEGAL %>% pivot_longer(cols = c("H","A","E"))

maxtime = max(FULLDATA_DENMARK$time)

### Too big for 100,000 
FULLDATA <- rbind(FULLDATA_DENMARK %>% mutate(ctry = "denmark"),#, runs = seq(1, dim(FULLDATA_DENMARK)[1])),
                 FULLDATA_ENGLAND %>% mutate(ctry = "england"),#, runs = seq(1, dim(FULLDATA_ENGLAND)[1])),
                 FULLDATA_SENEGAL %>% mutate(ctry = "senegal")) #%>% #, runs = seq(1, dim(FULLDATA_SENEGAL)[1])))

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

## Load function to calculate distance
source("0_model_functions.R")

### Explore distance from data for each country 
MLE_SENEGAL <- rep(NA,nruns)
MLE_ENGLAND <- rep(NA,nruns)
MLE_DENMARK <- rep(NA,nruns)


FULLDATA_SENEGAL_mean <- FULLDATA_SENEGAL %>% group_by(year, name, paraset) %>%
  summarise(mean_vl = mean(value)) %>%
  ungroup()

FULLDATA_ENGLAND_mean <- FULLDATA_ENGLAND %>% group_by(year, name, paraset) %>%
  summarise(mean_vl = mean(value)) %>%
  ungroup()

FULLDATA_DENMARK_mean <- FULLDATA_DENMARK %>% group_by(year, ctry, name, paraset) %>%
  summarise(mean_vl = mean(value)) %>%
  ungroup()



## Resistance data to fit to 
res.table <- read_csv("data/res.table.fit.csv")
ggplot(res.table, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))

### How far from data?
data_fit <- res.table %>% select(country,var,time,percent) %>% 
  rename(ctry = country, year = time, name = var) %>%
  mutate(proportion = percent / 100)

joined_fit_SENEGAL <- left_join(FULLDATA_SENEGAL_mean,data_fit) %>% filter(!is.na(proportion)) %>%
  ungroup() %>%
  mutate(distance = (proportion - mean_vl)^2)
dim(joined_fit_SENEGAL) # should be 72(size of data) x nruns

joined_fit_DENMARK <- left_join(FULLDATA_DENMARK_mean,data_fit) %>% filter(!is.na(proportion)) %>%
  ungroup() %>%
  mutate(distance = (proportion - mean_vl)^2)
dim(joined_fit_DENMARK) # should be 72(size of data) x nruns

joined_fit_ENGLAND <- left_join(FULLDATA_ENGLAND_mean,data_fit) %>% filter(!is.na(proportion)) %>%
  ungroup() %>%
  mutate(distance = (proportion - mean_vl)^2)
dim(joined_fit_ENGLAND) # should be 72(size of data) x nruns


ls_summ <- joined_fit %>% group_by(ctry, paraset) %>%
  summarise(dist = sum(distance))

### Store
write.csv(ls_summ, "output/LS_values.csv")

ggplot(ls_summ, aes(x=paraset, y = dist)) + geom_point(aes(col = ctry))
ggplot(ls_summ, aes(x=ctry, y = dist)) + geom_violin(aes(col = ctry)) # Worst fit for denmark

## This is the model output ##
# Find the max 100 and store
ls_top <- ls_summ %>% 
  group_by(ctry) %>%
  arrange(dist) %>% 
  slice(1:100) %>%
  ungroup()

ggplot(ls_top, aes(x=ctry, y = dist)) + geom_violin(aes(col = ctry)) # But best lowest for denmark

ls_top_sen <- ls_top %>% filter(ctry == "senegal") %>% select(paraset) %>% pull()
ls_top_eng <- ls_top %>% filter(ctry == "england") %>% select(paraset) %>% pull()
ls_top_den <- ls_top %>% filter(ctry == "denmark") %>% select(paraset) %>% pull()
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
                 "beta_HA","beta_EH","beta_EA","beta_AE","beta_HE","mu_H","mu_A","mu_E")
p$para <- seq(1, dim(p)[1],1)
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

ggplot(best, aes(x=year, y = value, group = paraset)) + geom_line() + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), col = "red") + 
  facet_wrap(ctry~name,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_0105.pdf")

ggplot(best, aes(x=year, y = value, group = paraset)) + geom_line() + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), col = "red") + 
  facet_wrap(name~ctry,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_flip_0105.pdf")



### Averages
fits_av <- best %>% group_by(year, ctry, name) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975)))

ggplot(fits_av, aes(x=year, y = mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = min025, ymax = max975)) + 
  geom_point(data = res.table %>% rename(name = var), aes(x=time, y = percent/100, col = name)) + 
  facet_wrap(ctry ~ name) + 
  guides(col="none")
ggsave("plots/fits_mean_quantile.pdf")

############# Visualise parameter outputs
best_100_para_senegal$ctry <- "senegal"
best_100_para_england$ctry <- "england"
best_100_para_denmark$ctry <- "denmark"
best_para <- rbind(best_100_para_senegal,
                   best_100_para_england,
                   best_100_para_denmark)

ggplot(best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E"), aes(x=name, y = value)) + 
  geom_violin(aes(fill=ctry),alpha = 0.2) +
  facet_wrap(~name, scales = "free", nrow = 3)

