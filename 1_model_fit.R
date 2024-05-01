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

### Usage
usage <- read.csv("data/input_usage.csv")

### Data to fit to 
res.data <- read.csv("data/res.table.fit.csv")

##### Generate parameter samples
#ONLY FIRST TIME
# set.seed(12345)
# p1<-sampling(500000) # go big to get lots as criterion remove lots
# ## Add final limitations
# p1 <- p1[p1[,11] >= p1[,9], ] # removes a lot
# ## Take 100,000 of these: do random as LHS aims to go over the whole space
# p1 <- p1[sample(x = dim(p1)[1], size = 100000),]
# dim(p1) # 100,000 parameter sets
# p1 <- cbind(p1, seq(1, dim(p1)[1],1))
# write.csv(p1,"output/parameter_set_100000.csv")
# p1p <- as.data.frame(p1) %>% mutate("para" = seq(1,dim(p1)[1],))
# colnames(p1p) <- c("LAMBDA_H","LAMBDA_A","LAMBDA_E","beta_HH","beta_AA","beta_EE","beta_AH",
#   "beta_HA","beta_EH","beta_EA","beta_AE","beta_HE","mu_H","mu_A","mu_E","para")
# ggplot(p1p %>% pivot_longer(cols = "LAMBDA_H":"mu_E"), aes(x=name, y = value)) + geom_violin() +
#   facet_wrap(~name, scales = "free")
# ggsave("output/parameter_sets.pdf")

p1 <- read.csv("output/parameter_set_100000.csv")[,-1]


### EXPLORE how to Run
times <- seq(1,(2022 - 1984)*12,1) # 1990 - 2022 monthly
# 
# A_big <- c()
# for(ii in 39500:39510){#dim(p1)[1]){
#   parameters <-as.numeric(p1[ii,])
#   A <- AMRmodel(times, c(1,0,0), usage %>% filter(country == "senegal"), parameters)
#   A$year <- c(1985,rep(seq(1985,1985 + 37,1),each = 12))
#   A$month <- c(1,rep(seq(1,12,1),38))
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

### FOR TIME RUN < 100,000
p1 <- p1[1:10000,]

### SENEGAL 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, "usage")
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - 1984)*12,1), c(0.01,0.01,0.01), usage %>% filter(country == "senegal"), as.numeric(x))) # for each country j

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
clusterExport(cl, "usage")
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - 1984)*12,1), c(0.01,0.01,0.01), usage %>% filter(country == "denmark"), as.numeric(x))) # for each country j

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
clusterExport(cl, "usage")
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) AMRmodel(seq(1,(2022 - 1984)*12,1), c(0.01,0.01,0.01), usage %>% filter(country == "england"), as.numeric(x))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/","england",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

