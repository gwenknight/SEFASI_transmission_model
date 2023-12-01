###################################################################################
################################    Run SEFASI transmission model    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())

#### Load needed functions
# Loads Core AMR model
source("model_functions/AMRmodel.R")
# Loads epid function: wrapper for core model 
source("model_functions/epid.R")
# Loads Sampling function: LHS sampling with checks on parameters
source("model_functions/sampling.R")
# Loads function to run across LHS parameters (outFun and temp one for non parallel)
source("model_functions/run_for_many_para.R")
# Plot output functions
source("plot_functions/plotfits.R")
source("plot_functions/plotfits2.R")

##### Generate parameter samples
set.seed(12345)
p1<-sampling(500000) # go big to get lots as criterion remove lots
## Add final limitations
p1 <- p1[p1$beta_EA >= p1$beta_EH, ] # removes a lot
## Take 100,000 of these: do random as LHS aims to go over the whole space
p1 <- p1[sample(x = dim(p1)[1], size = 100000),]
dim(p1) # 100,000 parameter sets

write.csv(p1,"output/parameter_set_100000.csv")

##### Generate time varying antibiotic usage curves
# will plot england and denmark assumptions for usage & generates time varying LAMBDA
source("plot_functions/explore_and_plot_time_varying_usage.R") 



### Raw data
den_use <- usage.table %>% filter(country == "denmark")

#### Add in missing data for 2020 and 2000
# # ?assumption leave out 2020 point  (8.9) because not comparable to just "cephalosporins"
## Assume 2020 levels same as 2019 as no data 
temp_2020_row_extra <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans" & usage.table$year == (2019),] #this is to make sure 2020 point fits into function assumption 2020= 2019
temp_2020_row_extra$year <- 2020

#for AMRmodel.R to make sense we need to add data before 2001: assume same in 2000 as in 2001
temp_2000_row_extra <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans" & usage.table$year == (2001),] #this is to make sure 2020 point fits into function assumption 2020= 2019
temp_2000_row_extra$year <- 2000
usage.table <- rbind(usage.table,temp_2020_row_extra,temp_2000_row_extra)

den_use_h <- usage.table[usage.table$country=="denmark" &usage.table$subsource=="all humans",c("year","kg")]
##################################### Run the simulator outFUN for the Latin-Hypercube samples generated above
# Not parallel
#ptm <- proc.time() #time run 
#outFUN_temp(p1[1:100,],"denmark" #the file name
#) 
#proc.time() - ptm

p1 <- read.csv("output/parameter_set_100000.csv")[,-1]

# Run in parallel
nc = detectCores()
source("model_functions/epid.R")


### SENEGAL 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
clusterExport(cl, c("epid","ode"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("model_functions/AMRModel.R"))
clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(p1, row(p1)), function(x) epid(x,0, "senegal")) # for each country j

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
clusterExport(cl, c("epid","ode"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("model_functions/AMRModel.R"))
clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <- parLapply(cl, split(p1, row(p1)), function(x) epid(x,0, "denmark")) # for each country j

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
clusterExport(cl, c("epid","ode"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("model_functions/AMRModel.R"))
clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <- parLapply(cl, split(p1, row(p1)), function(x) epid(x,0, "england")) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits/","england",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)



# ###### Not working  -> gives same results for Denmark and Senegal? 
# ### GK: computer 0.5s per run
# ptm <- proc.time() #time run 
# outFUN(p1[1:1000,],"england" #the file name 
# ) 
# proc.time() - ptm
# 
# ptm <- proc.time() #time run 
# outFUN(p1[1:1000,],"denmark" #the file name
# ) 
# proc.time() - ptm
# 
# ptm <- proc.time() #time run 
# outFUN(p1[1:1000,],"senegal" #the file name
# ) 
# proc.time() - ptm
# 
