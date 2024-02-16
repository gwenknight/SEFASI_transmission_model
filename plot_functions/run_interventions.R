###################################################################################
################################    Run interventions in SEFASI transmission model    #####################################
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
# Loads epid intervention function: wrapper for intervention model 
source("model_functions/epid_intervention.R")
# Loads paras
source("plot_functions/explore_and_plot_time_varying_usage.R")


#### Parameters for best fit (from explore_model_output.R)
best_para <- read_csv("output/best_parameter_sets.csv")


### SENEGAL 
# Make cluster
store_all <- as.data.frame(matrix(0,28 * 100 * 19 * 3,7))
summary_all <- as.data.frame(matrix(0, 100 * 19 * 3,15))
index <- 1
for(i in 1:100){ #for all parameter sets 
  
  for(j in 1:3){ # for all countries 
    
    country_run = c("senegal","denmark","england")[j]
    print(c(i,country_run))
    best_para_cntry <- best_para %>% filter(country == country_run)
    
    for(k in 1:19){ # for all interventions
      
        s <- epid_intervention(best_para_cntry[i,],1,k, country_run)
        
        # Store 
        store_all[((index-1)*28 + 1):((index-1)*28 + 28),] <- cbind(s$out2, country_run, i, k)
        summary_all[index,] <- c(s$summary, country_run, i, k)
        
        # Index update
        index <- index + 1
    }
    
  }
}

colnames(store_all) <- c(colnames(s$out2), "country","paraset","intervention")
colnames(summary_all) <- c(names(s$summary), "country","paraset","intervention")

write_csv(store_all, "output/interventions_estimates.csv")
write.csv(summary_all, "output/interventions_summary.csv")

###### IMPACT variance

labels_int <- c("No human AMU","No animal AMU","No environmental AMU",
                "No human-human transm.", "No animal-animal transm.", "No human-envir. transm.",
                "No animal-human transm.", "No envir.-human transm.", "No human-animal. transm.",
                "No envir.-animal transm.", "No animal-envir. transm.", "No envir.-envir. transm.",
                "10% reduced human-human transm.","25% reduced human-human transm. and human AMU",
                "20% reduced human and animal AMU, and all transm", "30% reduced human and animal AMU, and all transm", 
                "50% reduced envir. to human and animal transm.","50% reduced human <-> animal transm.", "50% reduced AMU")

summary_all
impact <- summary_all %>% select(differenceH_percent:intervention) %>% pivot_longer(cols = differenceH_percent:differenceE_percent) 
impact$value <- as.numeric(impact$value) 
impact$intervention <- as.numeric(impact$intervention) 
impact_summ <- impact %>% group_by(country, intervention, name) %>% summarise(mean_imp = mean(value), sd_imp = sd(value)) %>% mutate(environment = substring(substring(name, nchar(name)-8),1,1))

ggplot(impact_summ, aes(x=intervention, y = mean_imp, group = name)) + 
  geom_errorbar(aes(ymin = mean_imp - sd_imp, ymax = mean_imp + sd_imp)) + 
  facet_wrap(environment~country) + 
  scale_x_continuous(breaks = c(1,2,3,19,seq(4,18,1)), labels = labels_int[c(1,2,3,19,seq(4,18,1))]) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_vline(xintercept = c(4.5, 12.5), linetype = "dashed")


impact_summ[1:10,] %>% mutate()





# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(best_para %>% filter(country == "senegal"), 
                                      row(best_para %>% filter(country == "senegal"))), function(x) epid_intervention(x,0, 1, "senegal")) # for each country j

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

