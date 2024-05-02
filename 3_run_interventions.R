
###################################################################################
################################  SEFASI transmission model #####################################
################################# Run the interventions on the best fitting parameter sets #################################
################################# Ross Booton & Gwen Knight #####################################
################################## May 2024 #####################################
###################################################################################

rm(list = ls())
library(tidyverse)
library(data.table)
setwd(here())
theme_set(theme_bw())

## Functions
source("0_model_functions.R")


# Run the model on these parameters 
# for 20 years?
# set usage to be as it was in 2022 
end_usage <- usage %>% filter(country == "senegal") %>% 
  filter(year == max(u$year), month == 12)
new_usage <- matrix(0,20*12*dim(end_usage)[1],dim(end_usage)[2])
index <- 1
for(y in 1:20){
  for(m in 1:12){
    end_usage$year <- y
    end_usage$month <- m
    new_usage[(1+(index-1) * dim(end_usage)[1]):(index*dim(end_usage)[1]),] = end_usage
    index <- index + 1
  }
}




################################ boxplot of interventions  ################################ 
best_100_para_senegal <- read.csv("output/best_100_para_senegal.csv")
best_100_para_england <- read.csv("output/best_100_para_england.csv")
best_100_para_denmark <- read.csv("output/best_100_para_denmark.csv")

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




