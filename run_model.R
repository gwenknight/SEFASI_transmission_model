###################################################################################
################################    Run SEFASI transmission model    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())

############################## Input parameters 
input.table <- as.data.frame(read.csv("data/input.table.csv"))
input.table$parameter <- as.character(input.table$parameter)

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
p1<-sampling(500000) # go big to get lots as criterion remove lots
## Add final limitations
p1 <- p1[p1$beta_EA >= p1$beta_EH, ] # removes a lot
## Take 100,000 of these: do random as LHS aims to go over the whole space
p1 <- p1[sample(x = dim(p1)[1], size = 100000),]
dim(p1) # 100,000 parameter sets

##### Generate time varying antibiotic usage curves
# will plot england and denmark assumptions for usage & generates time varying LAMBDA
source("plot_functions/explore&plot_time_varying_usage.R") 


##################################### Run the simulator outFUN for the Latin-Hypercube samples generated above
# Not parallel
#ptm <- proc.time() #time run 
#outFUN_temp(p1[1:100,],"denmark" #the file name
#) 
#proc.time() - ptm

### GK: computer 0.5s per run
ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"england" #the file name 
) 
proc.time() - ptm

ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"denmark" #the file name
) 
proc.time() - ptm

ptm <- proc.time() #time run 
outFUN(p1[1:10000,],"senegal" #the file name
) 
proc.time() - ptm

