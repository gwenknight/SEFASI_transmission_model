#################################################################################
################################  SEFASI transmission model explore fits    #####
######### Gwen Knight based on initial work by Ross Booton ####################
########## May 2024 ###########################################################

##### WHEN DATASETS TOO BIG ########

rm(list = ls())
library(tidyverse)
library(data.table)
library(patchwork)
library(ggcorrplot) # for ggcorrplot()
library(here)
setwd(here())
theme_set(theme_bw(base_size =  11))

# initial values
source("0_initial_conditions.R")

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# Data to fit
res.table <- read.csv("data/res.table.fit.csv")
data_fit <- res.table %>% select(country,var,time,percent) %>% 
  rename(ctry = country, year = time, name = var) %>%
  mutate(proportion = percent / 100)


# Do for each country separately
for(i in 1:3){
  print(i)
  
  if(i == 1){FULLDATA <-fread("fits/denmark88500000.csv")[,-1]
  colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
  nruns = length(unique(FULLDATA$paraset))
  FULLDATA$year <- rep(c(init_denmark_year,rep(seq(init_denmark_year+1,2022,1),each = 52)),nruns)}
  if(i == 2){FULLDATA <-fread("fits/england109300000.csv")[,-1]
  colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
  nruns = length(unique(FULLDATA$paraset))
  FULLDATA$year <- rep(c(init_england_year,rep(seq(init_england_year+1,2022,1),each = 52)),nruns)}
  if(i == 3){FULLDATA <-fread("fits/senegal98900000.csv")[,-1]
  colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
  nruns = length(unique(FULLDATA$paraset))
  FULLDATA$year <- rep(c(init_senegal_year,rep(seq(init_senegal_year+1,2022,1),each = 52)),nruns)
  }
  
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
ggplot(ls_summ, aes(x=ctry, y = dist)) + geom_violin(aes(col = ctry)) 

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
FULLDATA <-fread("fits/denmark88500000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_denmark <- FULLDATA %>% filter(paraset %in% ls_top_den)
write.csv(best_100_denmark,"output/best_100_denmark.csv")
rm(FULLDATA)

FULLDATA <-fread("fits/england109300000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_england <- FULLDATA %>% filter(paraset %in% ls_top_eng)
write.csv(best_100_england,"output/best_100_england.csv")
rm(FULLDATA)

FULLDATA <-fread("fits/senegal98900000.csv")[,-1]
colnames(FULLDATA) <- c("time", "H", "A", "E", "paraset")
best_100_senegal <- FULLDATA %>% filter(paraset %in% ls_top_sen)
write.csv(best_100_senegal,"output/best_100_senegal.csv")

rm(FULLDATA)

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
source("0_initial_conditions.R")
res.table <- read_csv("data/res.table.fit.csv") %>% mutate(prop = percent / 100)
data_fit <- res.table %>% select(country,ctry, var,time,percent, source) %>% 
  rename(year = time, name = var) %>%
  mutate(proportion = percent / 100)

best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1]
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1]
best_100_england <- read_csv("output/best_100_england.csv")[,-1]

best <- rbind(best_100_senegal %>% mutate(ctry = "Senegal", year = rep(c(init_senegal_year,rep(seq(init_senegal_year+1,2022,1),each = 52)),100)),
              best_100_england %>% mutate(ctry ="England", year = rep(c(init_england_year,rep(seq(init_england_year+1,2022,1),each = 52)),100)),
              best_100_denmark %>% mutate(ctry ="Denmark", year = rep(c(init_denmark_year,rep(seq(init_denmark_year+1,2022,1),each = 52)),100))) %>%
  pivot_longer(cols = c("H","A","E"))

ggplot(best, aes(x=year, y = value, group = interaction(ctry,paraset))) + 
  geom_line(aes(col = ctry)) + 
  facet_wrap(ctry~name,ncol = 3) + 
  scale_color_manual("Country", breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) + 
  scale_y_continuous("Proportion resistant", lim = c(0,1)) + 
  scale_x_continuous("Year") + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), 
             col = "black")
ggsave("plots/model_fits.jpeg")

ggplot(best, aes(x=year, y = value, group = interaction(ctry,paraset))) + 
  geom_line(aes(col = ctry)) + 
  geom_point(data = data_fit,aes(x=year, y = proportion, group = ctry), col = "black") + 
  facet_wrap(name~ctry,ncol = 3) + 
  scale_y_continuous("Proportion resistant", lim = c(0,1)) + 
  scale_x_continuous("Year") + 
  scale_color_manual("Country", breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 

ggsave("plots/model_fits_flip.jpeg")



### Averages
fits_av <- best %>% group_by(year, ctry, name) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975)))

fits_av[which(fits_av$name == "A"),"name"] <- "Animal"
fits_av[which(fits_av$name == "H"),"name"] <- "Human"
fits_av[which(fits_av$name == "E"),"name"] <- "Environment"

data_fit[which(data_fit$name == "A"),"name"] <- "Animal"
data_fit[which(data_fit$name == "H"),"name"] <- "Human"
data_fit[which(data_fit$name == "E"),"name"] <- "Environment"

g1 <- ggplot(fits_av, aes(x=year, y = mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = min025, ymax = max975)) + 
  geom_point(data = data_fit, aes(x=year, y = percent/100, col = ctry, pch = source), 
             size = 2.5) + 
  facet_grid(ctry ~ name) + 
  guides(col="none") + 
  scale_y_continuous("Proportion resistance (over top 100 fits)") + 
  scale_x_continuous("Year") + 
  scale_shape_manual(values = c(5,17,10,15,16,4,9,6,7,8,3,18,1,2), 
                     breaks = c("EARS-NET", "DANMAP", "[Huijbers, 2020]", "UK-VARSS", "ESPAUR", 
                                "[Leonard, 2015]", "[Abdallah, 2022]", "[Vounba, 2018]", "[Vounba, 2019]", 
                                "[Diop-Ndiaye, 2014]", "Dakar Hospital data", "[Dramowski, 2021]", 
                                "[Breurec, 2016]", "[Ruppe, 2009]")) + 
  scale_color_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 

ggsave("plots/fig2.jpeg", width = 10, height = 5)


############# Visualise parameter outputs
best_100_para_senegal <- read_csv("output/best_100_para_senegal.csv")[,-1]
best_100_para_denmark <- read_csv("output/best_100_para_denmark.csv")[,-1]
best_100_para_england <- read_csv("output/best_100_para_england.csv")[,-1]

best_100_para_senegal$ctry <- "Senegal"
best_100_para_england$ctry <- "England"
best_100_para_denmark$ctry <- "Denmark"
best_para <- rbind(best_100_para_senegal,
                   best_100_para_england,
                   best_100_para_denmark)
write.csv(best_para, "output/best_para.csv")

g2a <- ggplot(best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E"), 
              aes(x=ctry, y = value)) + 
  geom_violin(aes(fill=ctry),alpha = 0.4) +
  facet_wrap(~name, scales = "free", nrow = 3) + 
  scale_x_discrete("", labels = c("","","")) + 
  scale_y_continuous("Best fit distribution") + 
  scale_fill_manual("Country",breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 
ggsave("plots/best_paras_0705.pdf")


### What about the beta parameters? 
g1 <- ggplot(best_para, aes(x=beta_HH, y = beta_AA, group = ctry)) + geom_point(aes(col = ctry)) + geom_smooth(method = lm, formula = y~x, aes(col = ctry, fill = ctry), alpha = 0.2) + scale_color_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) + scale_fill_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 
g2 <- ggplot(best_para, aes(x=beta_HH, y = beta_EE, group = ctry)) + geom_point(aes(col = ctry)) + geom_smooth(method = lm, formula = y~x, aes(col = ctry, fill = ctry), alpha = 0.2) + scale_color_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) + scale_fill_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 
g3 <- ggplot(best_para, aes(x=beta_EE, y = beta_AA, group = ctry)) + geom_point(aes(col = ctry)) + geom_smooth(method = lm, formula = y~x, aes(col = ctry, fill = ctry), alpha = 0.2) + scale_color_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) + scale_fill_manual(breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) 

g1 + g3 + g2 + guide_area() + plot_layout(guides = "collect")
ggsave("plots/beta_xx_para.pdf")
#cor(numeric_vars, use = "pairwise.complete.obs")

g1 <- ggcorrplot(tl.cex = 7,type = "lower",cor(best_para[which(best_para$ctry == "Senegal"),1:15],use = "pairwise.complete.obs")) + ggtitle("Senegal") 
g2 <- ggcorrplot(tl.cex = 7,type = "lower",cor(best_para[which(best_para$ctry == "Denmark"),1:15],use = "pairwise.complete.obs")) + ggtitle("Denmark")
g3 <- ggcorrplot(tl.cex = 7,type = "lower",cor(best_para[which(best_para$ctry == "England"),1:15],use = "pairwise.complete.obs")) + ggtitle("England")

p <- g1 + g2 + g3 + guide_area() + plot_layout(guides = "collect")
ggsave("plots/correlation_para.jpeg")

( g2a | p ) + plot_annotation(tag_levels = 'A') +  plot_layout(widths = c(3.5, 2.5))
ggsave("plots/fig3.jpeg", width = 16, height = 7)
