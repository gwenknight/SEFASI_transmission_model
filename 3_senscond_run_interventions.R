################################  SEFASI transmission model #######################
############# Run the interventions on the best fitting parameter sets ############
############# that fulfill the conditions in Booton et al ####################
################### Gwen Knight based on initial work by Ross Booton ##############
################################## May 2024 #####################################

rm(list = ls())
library(tidyverse)
library(data.table)
library(parallel)
library(doParallel)
library(patchwork)
library(here)
setwd(here())
theme_set(theme_bw())

## Functions
source("0_model_functions.R")

## initial conditions
source("0_initial_conditions.R")

## usage data
usage <- read.csv("data/input_usage.csv")

# Run the model on these parameters 
# time horizon
thor <- 5

# set usage to be as it was in 2022 
end_usage <- usage %>% group_by(country) %>% 
  filter(year == max(usage$year), week == 52) %>% select(-c(norm_year,time)) %>% as.matrix() 
new_usage <- matrix(0,thor*52*dim(end_usage)[1]*3,dim(end_usage)[2])
index <- 1
for(c in c("senegal","denmark","england")){
  for(y in 1:thor){
    for(m in 1:52){
      end_usage[,"year"] <- y
      end_usage[,"week"] <- m
      new_usage[((index-1) * dim(end_usage)[1]+1):(index*dim(end_usage)[1]),] = end_usage
      index <- index + 1
    }
  }
}
new_usage <- as.data.frame(new_usage)
colnames(new_usage) <- colnames(usage)[1:dim(new_usage)[2]]
# Check flat
#ggplot(new_usage, aes(x=year, y = normalise_kg, group = interaction(source,country))) + geom_line(aes(col = country))

######### run interventions
best_100_para_senegal <- as.data.frame(read.csv("output/best_100_para_senegal.csv"))[,-1]
best_100_para_england <- as.data.frame(read.csv("output/best_100_para_england.csv"))[,-1]
best_100_para_denmark <- as.data.frame(read.csv("output/best_100_para_denmark.csv"))[,-1]

######### parameter conditions 
best_100_para_senegal %>% filter(beta_HH > beta_AH, 
                                 beta_HH > beta_EH,
                                 beta_AA > beta_HA, 
                                 beta_AA > beta_EA,
                                 beta_HA < beta_AH, 
                                 beta_EH < beta_EA)
## only 1! 

best_100_para_denmark %>% filter(beta_HH > beta_AH, 
                                 beta_HH > beta_EH,
                                 beta_AA > beta_HA, 
                                 beta_AA > beta_EA,
                                 beta_HA < beta_AH, 
                                 beta_EH < beta_EA)
## only 4!

best_100_para_england %>% filter(beta_HH > beta_AH, 
                                 beta_HH > beta_EH,
                                 beta_AA > beta_HA, 
                                 beta_AA > beta_EA,
                                 beta_HA < beta_AH, 
                                 beta_EH < beta_EA)
## only 4!

# BTW best 100 england not the same as denmark: 
best_100_para_england[,1] - best_100_para_denmark[,1]

## As so few can't do this as a sensitivity analysis 