
###################################################################################
################################  SEFASI transmission model #####################################
################################# Run the interventions on the best fitting parameter sets #################################
################################# Ross Booton & Gwen Knight #####################################
################################## May 2024 #####################################
###################################################################################

rm(list = ls())
library(tidyverse)
library(data.table)
library(parallel)
library(doParallel)
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
best_100_para_senegal <- as.matrix(read.csv("output/best_100_para_senegal.csv"))[,-1]
best_100_para_england <- as.matrix(read.csv("output/best_100_para_england.csv"))[,-1]
best_100_para_denmark <- as.matrix(read.csv("output/best_100_para_denmark.csv"))[,-1]

nc = detectCores()
### SENEGAL 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","new_usage","best_100_para_senegal","init_senegal","init_senegal_year"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(best_100_para_senegal, row(best_100_para_senegal)), function(x) AMRmodel_interv(seq(1,(2022 - init_senegal_year)*52,1), init_senegal, 
                                                                                                                      usage %>% filter(country == "senegal", year >= init_senegal_year), 
                                                                                                                      new_usage %>% filter(country == "senegal"), as.numeric(x))) # for each country j

# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits_interv/","senegal",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)


################################ boxplot of interventions  ################################ 
best_100_para_senegal <- read.csv("output/best_100_para_senegal.csv")[,-1]
best_100_para_england <- read.csv("output/best_100_para_england.csv")[,-1]
best_100_para_denmark <- read.csv("output/best_100_para_denmark.csv")[,-1]

se <- plotfits_int_box(best_100_para_england,"england")
sd <- plotfits_int_box(best_100_para_denmark,"denmark")
ss <- plotfits_int_box(best_100_para_senegal,"senegal")

se <- as.data.frame(se) %>% mutate(country = "england")
sd <- as.data.frame(sd) %>% mutate(country = "denmark")
ss <- as.data.frame(ss) %>% mutate(country = "senegal")

impact_all <- rbind(se, sd, ss)
write.csv(impact_all, "output/impact_all.csv")

# Explore impact
ggplot(impact_all %>% filter(intervention > 11), 
       aes(x=intervention, y = differenceH, group = intervention)) + 
  geom_boxplot() + 
  facet_wrap(~country) + 
  scale_x_continuous(breaks = seq(12,20,1),
                     labels = c("Denmark NAP","England NAP", "Senegal NAP",
                                "Farm","Human","Environment","Human + Animal contact", 
                                "Transmission", "Usage")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  scale_y_continuous("Difference in human prevalence")
ggsave("plots/interventionH_boxplot_all.pdf")

### Total differences
impact_all$total_diff = impact_all$differenceH + impact_all$differenceE + impact_all$differenceA

ggplot(impact_all %>% filter(intervention > 11), 
       aes(x=intervention, y = total_diff, group = intervention)) + 
  geom_boxplot() + 
  facet_wrap(~country) + 
  scale_x_continuous(breaks = seq(12,20,1),
                     labels = c("Denmark NAP","England NAP", "Senegal NAP",
                                "Farm","Human","Environment","Human + Animal contact", 
                                "Transmission", "Usage")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  scale_y_continuous("Difference in total AMR prevalence")
ggsave("plots/interventionT_boxplot_all.pdf")

### Not just on human
impact_all_long <- impact_all %>% pivot_longer(differenceH:differenceE_percent)

ggplot(impact_all_long %>% filter(intervention > 11), aes(x=intervention, 
                                                          y = value, 
                                                          group = interaction(intervention,name))) + 
  facet_wrap(country~name, nrow = 3, scales = "free") + 
  geom_boxplot() + 
  scale_x_continuous(breaks = seq(12,20,1),
                     labels = c("Denmark NAP","England NAP", "Senegal NAP",
                                "Farm","Human","Environment","Human + Animal contact", "Transmission", "Usage")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  scale_y_continuous("Difference in prevalence")
ggsave("plots/intervention_boxplot_all_3indicators_all.pdf", width = 20, height = 10)

#source("combined_plots.R") #produces the plots by interventiona and country




