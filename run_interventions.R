### ### Run the interventions on the best fitting parameter sets
### Load functions to run interventions 
source("model_functions/AMRmodel.R") # model function
source("model_functions/epid.R") # wrapper for model function
source("plot_functions/explore_and_plot_time_varying_usage.R") # abx usage
source("model_functions/epid_intervention.R") # model function with interventions
source("plot_functions/plotfits_int.R")

## What is this? 
#sup_inf_data <- read.csv("output/sup_inf_data")
#plotfits_int(best_100_england,"england",0, sup_inf_data)


################################ boxplot of interventions  ################################ 
getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white","black")

source("plotfits_int_box.R")

plotfits_int_box(best_100_para_england,"england")
plotfits_int_box(best_100_para_denmark,"denmark")
plotfits_int_box(best_100_para_senegal,"senegal")

source("combined_plots.R") #produces the plots by interventiona and country




