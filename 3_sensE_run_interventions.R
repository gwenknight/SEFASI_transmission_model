
###################################################################################
################################  SEFASI transmission model #####################################
################################# Run the interventions on the best fitting parameter sets #################################
################################# EXCLUDING ENVIRONMENT FROM MODEL FIT #######################
################################# Ross Booton & Gwen Knight #####################################
################################## May 2024 #####################################
###################################################################################

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
best_100_para_senegal <- as.matrix(read.csv("output/sensE_best_100_para_senegal.csv"))[,-1]
best_100_para_england <- as.matrix(read.csv("output/sensE_best_100_para_england.csv"))[,-1]
best_100_para_denmark <- as.matrix(read.csv("output/sensE_best_100_para_denmark.csv"))[,-1]

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
write.csv(output_matrix, paste0("fits_interv/sensE_","senegal",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### DENMARK 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","new_usage","best_100_para_denmark","init_denmark","init_denmark_year"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(best_100_para_denmark, row(best_100_para_denmark)), function(x) AMRmodel_interv(seq(1,(2022 - init_denmark_year)*52,1), init_denmark, 
                                                                                                                      usage %>% filter(country == "denmark", year >= init_denmark_year), 
                                                                                                                      new_usage %>% filter(country == "denmark"), as.numeric(x))) # for each country j
# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits_interv/sensE_","denmark",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

### ENGLAND 
# Make cluster
cl = makeCluster(nc-3)
registerDoParallel(cl)

# Export things to the cluster
#clusterExport(cl, c("epid","ode"))
clusterExport(cl, c("usage","new_usage","best_100_para_england","init_england","init_england_year"))
clusterEvalQ(cl, library("tidyverse", character.only = TRUE))
clusterEvalQ(cl, source("0_model_functions.R"))
#clusterEvalQ(cl, source("plot_functions/explore_and_plot_time_varying_usage.R"))

# Use the parLapply function to run your function in parallel
output_results <-""
output_results <- parLapply(cl, split(best_100_para_england, row(best_100_para_england)), function(x) AMRmodel_interv(seq(1,(2022 - init_england_year)*52,1), init_england, 
                                                                                                                      usage %>% filter(country == "england", year >= init_england_year), 
                                                                                                                      new_usage %>% filter(country == "england"), as.numeric(x))) # for each country j
# Unlist and bind the results into a matrix
output_matrix <- do.call(rbind, output_results)

# Save them 
write.csv(output_matrix, paste0("fits_interv/sensE_","england",dim(output_matrix)[1],".csv"))

# Stop the cluster
stopCluster(cl)

############################################################################################################################################
##################################################################################################################################
########## EXPLORE outputs 
interv_senegal <- read.csv("fits_interv/sensE_senegal2472500.csv")[,-1]
interv_england <- read.csv("fits_interv/sensE_england2732500.csv")[,-1]
interv_denmark <- read.csv("fits_interv/sensE_denmark2212500.csv")[,-1]
interv_senegal$country <- "senegal"
interv_england$country <- "england"
interv_denmark$country <- "denmark"

interv <- rbind(interv_senegal, interv_denmark, interv_england) %>%
  filter(!para == 0)
interv_0 <- interv %>% filter(interven == 0) %>% rename(baseline = interven,
                                                        H0 = H, A0 = A, E0 = E)

interv_rel <- left_join(interv %>% filter(interven > 0), interv_0) %>%
  mutate(diffH = H0 - H,
         diffA = A0 - A,
         diffE = E0 - E)

interv_rel[which(interv_rel$country == "denmark"),"country"] <- "Denmark"
interv_rel[which(interv_rel$country == "senegal"),"country"] <- "Senegal"
interv_rel[which(interv_rel$country == "england"),"country"] <- "England"
write.csv(interv_rel,"output/sensE_interv_rel.csv")



# After 5 yrs, how has resistance change? diff = absolute proportion. Perc = percentage reduction 
interv_rel_5yr <- interv_rel %>% filter(time > 259) %>%  #filter(time == 5 * 52) %>% 
  rowwise() %>% 
  mutate(percH = 100 * diffH/H0,
         percA = 100 * diffA/A0,
         percE = 100 * diffE/E0) 
# Can do a total? what does this mean? 
#interv_rel_tot <- interv_rel %>% group_by(para, interven, country) %>% summarise(sum(diff))

# Only one that is negative is Senegal package = interven 14
iii <- interv_rel_5yr %>% filter(percH < 0) 
unique(iii$interven)
interv_rel_5yr <- interv_rel_5yr %>% 
  mutate(percH_n = ifelse(percH < 0, 0, NA),
         percA_n = ifelse(percA < 0, 0, NA),
         percE_n = ifelse(percE < 0, 0, NA))

######### 
intervention_names <- c("H abx to zero","A abx to zero", "E. abx to zero",
                        "HH spread to zero", "AA spread to zero", "EE spread to zero",
                        "A-H spread to zero","EH spread to zero","E-H spread to zero",
                        "E-A spread to zero","EA spread to zero",
                        "Denmark package","England package","Senegal package",
                        "Farm target","H target","E target",
                        "A-H spread down 50%","Spread down 50%","Abx down 50%",
                        "Spread down 30%","Spread down 20%","Spread down 10%", "Abx animals down 30%")


## Explore stability after 5 yrs 
ggplot(interv_rel %>% filter(para %in% c(14,186), 
                             interven %in% c(16,20)), 
       aes(x=time, y = H, group = interven)) + 
  geom_line(aes(col = factor(interven))) + 
  facet_wrap(~country, scales = "free") + 
  scale_color_discrete(breaks = c(16,20), 
                       labels = intervention_names[c(16,20)],"Interventions")


ggplot(interv_rel %>% filter(para %in% c(14,186), interven %in%  c(seq(12,20,1),24)), 
       aes(x=time, y = H, group = interven)) + 
  geom_line(aes(col = factor(interven))) + 
  facet_wrap(~country, scales = "free") + 
  scale_color_discrete(breaks =  c(seq(12,20,1),24), 
                       labels = intervention_names[c(12:20,24)],"Interventions")
ggsave("plots/sensE_time_trace_eg_interventions_H.pdf")


ggplot(interv_rel_5yr, aes(x=interven, y = diffH, group = interven)) + 
  ggtitle("Difference at 5yrs in humans") + 
  geom_boxplot(aes(fill = factor(interven))) + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_continuous(breaks = seq(1,24,1), labels = intervention_names,"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("darkslategray","darkslategray","darkslategray", # abx targe
                               "lightgreen","lightgreen","lightgreen", # spread targe
                               "lightgreen","lightgreen","lightgreen",
                               "lightgreen","lightgreen",
                               "chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "lightgreen","lightgreen","lightgreen",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sensE_all_interventions_H.pdf")

gi1 <- ggplot(interv_rel_5yr %>% filter(interven %in% c(seq(12,20,1),24)), aes(x=factor(interven), y = diffH, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  #ggtitle("Difference at 5yrs in humans") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sensE_ab_diff.jpeg")

gi2 <- ggplot(interv_rel_5yr %>% filter(interven%in%  c(seq(12,20,1),24)), aes(x = factor(interven), y = percH, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  # ggtitle("Difference at 5yrs in humans") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = c(0,100)) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Percentage reduction in proportion resistant at 5yrs")
ggsave("plots/sensE_interv_in_paper_Hperc_supp.pdf")

gi2 <- ggplot(interv_rel_5yr %>% filter(interven%in%  c(seq(12,20,1),24)), aes(x = factor(interven), y = percH, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  # ggtitle("Difference at 5yrs in humans") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = c(0,100)) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Percentage reduction in proportion resistant at 5yrs",limits = c(-0.01,100)) + 
  geom_point(data = interv_rel_5yr %>% filter(!is.na(percH_n)), aes(x = factor(interven), y = percH_n), pch = "*", size = 6)
ggsave("plots/sensE_interv_in_paper_Hperc_.pdf")

gi1 + gi2 + plot_annotation(tag_levels = "A")
ggsave("plots/sensE_fig4.jpeg", width = 12, height = 7)

### Animals
ggplot(interv_rel_5yr %>% filter(interven%in% c(seq(12,20,1),24)), aes(x = factor(interven), y = diffA, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  ggtitle("Difference at 5yrs in animals") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sensE_interv_in_paper_A.pdf")

ggplot(interv_rel_5yr %>% filter(interven%in% c(seq(12,20,1),24)), aes(x = factor(interven), y = percA, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  # ggtitle("Difference at 5yrs in humans") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = c(0,100)) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Percentage reduction in proportion resistant at 5yrs",limits = c(-0.01,100)) + 
  geom_point(data = interv_rel_5yr %>% filter(!is.na(percA_n)), aes(x = factor(interven), y = percA_n), pch = "*", size = 6)
ggsave("plots/sensE_interv_in_paper_Aperc.pdf")

### Environment
ggplot(interv_rel_5yr %>% filter(interven%in% c(seq(12,20,1),24)), aes(x = factor(interven), y = diffE, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  ggtitle("Difference at 5yrs in environment") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sensE_interv_in_paper_E.pdf")

ggplot(interv_rel_5yr %>% filter(interven%in% c(seq(12,20,1),24)), aes(x = factor(interven), y = percE, group = interven)) + 
  geom_boxplot(aes(fill = factor(interven))) + 
  # ggtitle("Difference at 5yrs in humans") + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_discrete(breaks = c(seq(12,20,1),24), labels = intervention_names[c(12:20,24)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "darkslategray")) + # abx targe
  coord_flip() + 
  geom_hline(yintercept = c(0,100)) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Percentage reduction in proportion resistant at 5yrs",limits = c(-0.01,100)) + 
  geom_point(data = interv_rel_5yr %>% filter(!is.na(percE_n)), aes(x = factor(interven), y = percE_n), pch = "*", size = 6)
ggsave("plots/sensE_interv_in_paper_Eperc.pdf")


#### Table 
table_res <- interv_rel_5yr %>% filter(interven %in% c(seq(12,20,1),24)) %>%
  ungroup() %>% 
  mutate(i_name = rep(intervention_names[c(12:20,24)],300)) %>% 
  group_by(country, interven) %>% 
  select(para, interven, country, i_name, percH, percA, percE) %>% 
  pivot_longer(cols = c("percH", "percA", "percE")) %>% 
  group_by(interven, country, name, i_name) %>% 
  summarise(mean = round(median(value),1), 
            low = round(quantile(value, probs = 0.025),1),
            high = round(quantile(value, probs = 0.975),1)) 
write.csv(table_res, "output/sensE_table_res.csv")
# For tables in paper
table_res_out <- table_res %>%
  mutate(out = paste0(mean, "% (",low,"%, ", high,"%)")) %>% 
  select(country, i_name, out) %>% 
  pivot_wider(names_from = country, values_from = out) %>%
  arrange(name)
write.csv(table_res_out, "output/sensE_table_res_for_paper.csv")


table_res <- table_res %>% mutate(Setting = "")
table_res[which(table_res$name == "percA"),"Setting"] <-  "Animals"
table_res[which(table_res$name == "percH"),"Setting"] <-  "Humans"
table_res[which(table_res$name == "percE"),"Setting"] <-  "Environment"

ggplot(table_res, aes(x=i_name, y = mean, group = country)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = country)) + 
  geom_errorbar(position = "dodge",aes(ymin = low, ymax = high, group = country)) + 
  scale_x_discrete("Intervention") + 
  facet_wrap(~Setting) + 
  scale_fill_manual("Country", breaks = c("England", "Denmark", "Senegal"), values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous("Percentage reduction", lim = c(0,100)) + 
  theme(legend.position="bottom")
ggsave("plots/sensE_intervention_impact_by_setting.jpeg", width = 15, height = 5)

ggplot(table_res, aes(x=i_name, y = mean, group = Setting)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = Setting)) + 
  geom_errorbar(position = "dodge",aes(ymin = low, ymax = high, group = Setting)) + 
  scale_x_discrete("Intervention") + 
  facet_wrap(~country) + 
  scale_fill_manual("Setting", breaks = c("Animals", "Humans", "Environment"), values = c("brown3","cornflowerblue","darkgoldenrod1")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous("Percentage reduction", lim = c(0,100)) + 
  theme(legend.position="bottom")
ggsave("plots/sensE_fig5.jpeg", width = 15, height = 5)
