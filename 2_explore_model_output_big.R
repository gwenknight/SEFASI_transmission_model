###################################################################################
################################    SEFASI transmission model explore fits    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  May 2024 #####################################
###################################################################################
##### WHEN DATASETS TOO BIG ########

rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
setwd(here())
theme_set(theme_bw())

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# Data to fit
data_fit <- res.table %>% select(country,var,time,percent) %>% 
  rename(ctry = country, year = time, name = var) %>%
  mutate(proportion = percent / 100)


# Do for each country separately
for(i in 1:3){
  
  if(i == 1){FULLDATA <-fread("fits/denmark45700000.csv")[,-1]}
  if(i == 2){FULLDATA <-fread("fits/england45700000.csv")[,-1]}
  if(i == 3){FULLDATA <-fread("fits/senegal45700000.csv")[,-1]}
  
  colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
  
  nruns = length(unique(FULLDATA$paraset))
  
  FULLDATA$year <- rep(c(1984,rep(seq(1985,2022,1),each = 12)),nruns)
  FULLDATA <- FULLDATA %>% pivot_longer(cols = c("H","A","E"))
  
  maxtime = max(FULLDATA$time)
  
  ################################ EXPLORE ###########################################
  ggplot(FULLDATA %>% filter(paraset < 5), aes(x=time, y = value, group = paraset)) + geom_line(aes(col = factor(paraset))) + 
    facet_grid( ~ name)
  
  ################################  FITTING DATA #####################################
  ## Load function to calculate distance
  #source("0_model_functions.R")
  
  ### Explore distance from data for each country 
  dist <- rep(NA,nruns)
  
  FULLDATA_mean <- FULLDATA %>% group_by(year, name, paraset) %>%
    summarise(mean_vl = mean(value)) %>%
    ungroup()
  ## Resistance data to fit to 
  res.table <- read_csv("data/res.table.fit.csv")
  
  ### How far from data?
  if(i == 1){data_fitc <- data_fit %>% filter(ctry == "denmark")}
  if(i == 2){data_fitc <- data_fit %>% filter(ctry == "england")}
  if(i == 3){data_fitc <- data_fit %>% filter(ctry == "senegal")}
  
  joined_fit <- left_join(FULLDATA_mean,data_fitc) %>% filter(!is.na(proportion)) %>%
    ungroup() %>%
    mutate(distance = (proportion - mean_vl)^2)
  dim(joined_fit) # should be 72(size of data) x nruns
  
  ls_summ <- joined_fit %>% group_by(ctry, paraset) %>%
    summarise(dist = sum(distance))
  
  ### Store
  if(i == 1){write.csv(ls_summ, "output/LS_values_DENMARK.csv")}
  if(i == 2){write.csv(ls_summ, "output/LS_values_ENGLAND.csv")}
  if(i == 3){write.csv(ls_summ, "output/LS_values_SENEGAL.csv")}
  
}
rm(FULLDATA)
rm(FULLDATA_mean)


#### After that bring together
ls_summd <- as.data.frame(read.csv("output/LS_values_DENMARK.csv")) %>% mutate(ctry = "Denmark")
ls_summe <- as.data.frame(read.csv("output/LS_values_ENGLAND.csv")) %>% mutate(ctry = "England")
ls_summs <- as.data.frame(read.csv("output/LS_values_SENEGAL.csv")) %>% mutate(ctry = "Senegal")

ls_summ <- rbind(ls_summd, ls_summe, ls_summs)

#ggplot(ls_summ, aes(x=paraset, y = dist)) + geom_point(aes(col = ctry))
ggplot(ls_summ, aes(x=ctry, y = dist)) + geom_violin(aes(col = ctry)) # Worst fit for denmark

## This is the model output ##
# Find the max 100 and store
ls_top <- ls_summ %>% 
  group_by(ctry) %>%
  arrange(dist) %>% 
  slice(1:100) %>%
  ungroup()

ggplot(ls_top, aes(x=ctry, y = dist)) + geom_violin(aes(col = ctry)) # But best lowest for denmark

ls_top_sen <- ls_top %>% filter(ctry == "Senegal") %>% select(paraset) %>% pull()
ls_top_eng <- ls_top %>% filter(ctry == "England") %>% select(paraset) %>% pull()
ls_top_den <- ls_top %>% filter(ctry == "Denmark") %>% select(paraset) %>% pull()


# Find best outputs for the top parasets 
FULLDATA <-fread("fits/denmark45700000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_denmark <- FULLDATA %>% filter(paraset %in% ls_top_den)
write.csv(best_100_denmark,"output/best_100_denmark.csv")

FULLDATA <-fread("fits/england45700000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_england <- FULLDATA %>% filter(paraset %in% ls_top_eng)
write.csv(best_100_england,"output/best_100_england.csv")

FULLDATA <-fread("fits/senegal45700000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_senegal <- FULLDATA %>% filter(paraset %in% ls_top_sen)
rm(FULLDATA)
write.csv(best_100_senegal,"output/best_100_senegal.csv")



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

best <- rbind(best_100_senegal %>% mutate(country = "senegal", year = rep(c(1984,rep(seq(1985,2022,1),each = 12)),100)),
              best_100_england %>% mutate(country ="england", year = rep(c(1984,rep(seq(1985,2022,1),each = 12)),100)),
              best_100_denmark %>% mutate(country ="denmark", year = rep(c(1984,rep(seq(1985,2022,1),each = 12)),100))) %>%
  pivot_longer(cols = c("H","A","E"))

ggplot(best, aes(x=year, y = value, group = paraset)) + geom_line() + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), col = "red") + 
  facet_wrap(ctry~name,ncol = 3) + 
  scale_y_continuous("Proportion resistant", lim = c(0,1)) + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_0205.pdf")

ggplot(best, aes(x=year, y = value, group = paraset)) + geom_line() + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), col = "red") + 
  facet_wrap(name~ctry,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_flip_0205.pdf")



### Averages
fits_av <- best %>% group_by(year, country, name) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975)))

ggplot(fits_av, aes(x=year, y = mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = min025, ymax = max975)) + 
  geom_point(data = res.table %>% rename(name = var), aes(x=time, y = percent/100, col = name)) + 
  facet_wrap(country ~ name) + 
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
ggsave("plots/best_paras.pdf")
