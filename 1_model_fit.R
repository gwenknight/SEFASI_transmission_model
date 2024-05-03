### Run model fit to data 

### libraries
library(tidyverse)
library(here)
library(parallel)
library(doParallel)
#library(desolve)
library(lhs)
rm(list = ls())
setwd(here())

## Functions
source("0_model_functions.R")
## Initial conditions
source("0_initial_conditions.R")

### Usage
usage <- read.csv("data/input_usage.csv")

##### Generate parameter samples
#### ONLY FIRST TIME
#set.seed(12345)
# p1<-sampling(100000) # go big to get lots as criterion remove lots
# dim(p1) # 100,000 parameter sets
# p1 <- cbind(p1, seq(1, dim(p1)[1],1))
# p1 <- as.data.frame(p1)
# colnames(p1) <- c("LAMBDA_H","LAMBDA_A","LAMBDA_E","beta_HH","beta_AA","beta_EE","beta_AH",
#   "beta_HA","beta_EH","beta_EA","beta_AE","beta_HE","mu_H","mu_A","mu_E","para")
# write.csv(p1,"output/parameter_set_100000.csv")
# ggplot(p1 %>% pivot_longer(cols = "LAMBDA_H":"mu_E"), aes(x=name, y = value)) + geom_violin() +
#   facet_wrap(~name, scales = "free")
# ggsave("output/parameter_sets.pdf")

p1 <- read.csv("output/parameter_set_100000.csv")[,-1]

### EXPLORE how to Run for SENEGAL
## running from different years for each now now 
# times <- seq(1,(2022 - init_senegal_year)*52,1) # start - 2022 weekly
# 
# A_big <- c()
# for(ii in 39500:39510){#dim(p1)[1]){
#   parameters <-as.numeric(p1[ii,])
#   A <- AMRmodel(times, init_senegal, usage %>% filter(country == "senegal", year >= init_senegal_year), parameters)
#   A$year <- c(init_senegal_year,rep(seq(init_senegal_year,init_senegal_year + 2022 - init_senegal_year-1,1),each = 52))
#   A$week <- c(1,rep(seq(1,52,1),2022 - init_senegal_year))
#   A$para <- ii
#   # Store
#   A_big <- rbind(A_big, A)
# }
#
# # Explore output
# A_big_o <- A_big # store
# A_big <- as.data.frame(A_big) %>% pivot_longer(cols = c("H","A","E"))
# 
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
#p1 <- p1[1:10,]

### SENEGAL 
# Make cluster
cl = makeCluster(nc-1)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_senegal","init_senegal_year"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - init_senegal_year)*52,1), init_senegal, usage %>% filter(country == "senegal", year >= init_senegal_year), as.numeric(x))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/","senegal",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### DENMARK 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_denmark_year","init_denmark"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - init_denmark_year)*52,1), init_denmark, usage %>% filter(country == "denmark", year >= init_denmark_year), as.numeric(x))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/","denmark",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### ENGLAND 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","init_england_year","init_england"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - init_england_year)*52,1), init_england, usage %>% filter(country == "england", year >= init_england_year), as.numeric(x))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/","england",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

