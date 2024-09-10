######### Run model fit to data (sensitivity analysis) ####################### 
######### WITH VARYING TRANSMISSION OVER TIME ################################ 
######### Gwen Knight  #######################################################
########## May 2024 ##########################################################


### libraries
library(tidyverse)
library(here)
library(parallel)
library(doParallel)
library(lhs)
rm(list = ls())
setwd(here())

## Functions
source("0_model_functions.R")
## Initial conditions
source("0_initial_conditions.R")

### Usage
usage <- read.csv("data/input_usage.csv")

### Transmission AHHA
transmission_ahha <- read.csv("data/transmission_ahha.csv")[,-1]

## Parameter samples
p1 <- read.csv("output/parameter_set_100000.csv")[,-1]

### EXPLORE how to Run for ENGLAND
# running from different years for each now now
# times <- seq(1,(2022 - init_england_year)*52,1) # start - 2022 weekly
# 
# A_big <- c()
# for(ii in 39500:39510){#dim(p1)[1]){
#   parameters <-as.numeric(p1[ii,])
#   A <- AMRmodel_transmssion(times, init_england, usage %>% filter(country == "england", year >= init_england_year), parameters,
#                             transmission_ahha %>% filter(country == "England", year >= init_england_year))
#   A$year <- c(init_england_year,rep(seq(init_england_year,init_england_year + 2022 - init_england_year-1,1),each = 52))
#   A$week <- c(1,rep(seq(1,52,1),2022 - init_england_year))
#   A$para <- ii
#   # Store
#   A_big <- rbind(A_big, A)
# }
# 
# # Explore output
# A_big_o <- A_big # store
# A_big <- as.data.frame(A_big) %>% pivot_longer(cols = c("H","A","E"))
# 
# ggplot(A_big, aes(x=time, y = value, group = para)) + geom_line(aes(col = factor(para))) +
#   facet_wrap(~name)
# 
# ggplot(A_big, aes(x=time, y = value, group = para)) + geom_line(aes(col = factor(para))) +
#   facet_wrap(~name) +
#   scale_y_continuous(lim = c(0,1))
# 
# A_big_mean <- A_big %>% group_by(year, name, para) %>%
#   summarise(mean_val = mean(value))
# 
# ggplot(A_big_mean, aes(x=year, y = mean_val, group = para)) + geom_line(aes(col = factor(para))) +
#   facet_wrap(~name) +
#   scale_y_continuous(lim = c(0,1))


############################## Run in parallel
nc = detectCores()
# Read in parameters 
p1 <- as.matrix(read.csv("output/parameter_set_100000.csv")[,-1])

### FOR TIME, RUN < 100,000
#p1 <- p1[1:1000,]

### SENEGAL 
# Make cluster
cl = makeCluster(nc-1)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_senegal","init_senegal_year","transmission_ahha"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel_transmssion(seq(1,(2022 - init_senegal_year)*52,1), init_senegal, usage %>% filter(country == "senegal", year >= init_senegal_year), as.numeric(x),
                                                                                     transmission_ahha %>% filter(country == "Senegal", year >= init_senegal_year))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/senstahha_","senegal",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### DENMARK 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_denmark_year","init_denmark","transmission_ahha"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel_transmssion(seq(1,(2022 - init_denmark_year)*52,1), init_denmark, usage %>% filter(country == "denmark", year >= init_denmark_year), as.numeric(x),
                                                                                     transmission_ahha %>% filter(country == "Denmark", year >= init_denmark_year))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/senstahha_","denmark",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### ENGLAND 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_england_year","init_england","transmission_ahha"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel_transmssion(seq(1,(2022 - init_england_year)*52,1), init_england, usage %>% filter(country == "england", year >= init_england_year), as.numeric(x),
                                                                                     transmission_ahha %>% filter(country == "England", year >= init_england_year))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/senstahha_","england",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

