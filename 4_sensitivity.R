#### Run univariate sensitivity analysis to explore sensitivity of output to parameters

rm(list = ls())
library(tidyverse)
library(patchwork)
library(here)
setwd(here())
theme_set(theme_bw())

# intervention impact 
interv_rel <- read.csv("output/interv_rel.csv")[,-1]
interv_rel_5yr <- interv_rel %>% filter(time == 260) %>% 
  rowwise() %>% 
  mutate(percH = 100 * diffH/H0,
         percA = 100 * diffA/A0,
         percE = 100 * diffE/E0) 

# parameters
p1 <- read.csv("output/parameter_set_100000.csv")[,-1]

## Run sensitivity on intervention impact 
# Want to know how much the parameters affect baseline level 
# Of those 100 best parameters, how much is intervention affected? 
best_para <- read.csv("output/best_para.csv")[,-1]

para_range_max <- best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E") %>%
  group_by(ctry, name) %>%
  summarise(maxval = max(value),
            para = ifelse(value == maxval,para,0)) %>%
  filter(para > 0) %>%
  pivot_longer(col = maxval, names_to = "limit", values_to = "value") %>% 
  rename(country = ctry)

para_range_min <- best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E") %>%
  group_by(ctry, name) %>%
  summarise(minval = min(value),
            para = ifelse(value == minval,para,0)) %>%
  filter(para > 0) %>% 
  pivot_longer(col = minval, names_to = "limit", values_to = "value") %>% 
  rename(country = ctry)

pp <- rbind(para_range_max, para_range_min)

gp <- left_join(pp, interv_rel_5yr[,c("country","para","interven","H0","percH")]) %>% 
  group_by(country, interven, limit) %>%
  arrange(H0)

# Baseline analysis 
widthval = 0.8
gp_baseline <- gp %>% filter(interven == 1) %>% # H0 same for all interventions
  select(country, name, limit, H0, interven) %>% pivot_wider(names_from = limit, values_from = H0) %>%
  mutate(width = abs(maxval - minval)) %>%
  group_by(country) %>%
  arrange(country, width) %>%
  ungroup() %>%
  mutate(para_number = rep(seq(1,15,1),3)) %>%
  mutate(xmin = para_number - widthval/2,
         xmax = para_number + widthval/2)

g1 <- ggplot(gp_baseline) + geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
  theme(axis.text = element_text(size=12),
        axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  facet_wrap( ~ country, scales = "free") +
  scale_fill_manual(breaks =c("LAMBDA_H", "LAMBDA_A", "LAMBDA_E", 
                              "beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E"),
                    values = c("#ABDDA4","#66C2A5","lightgreen", 
                               "red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue")) + 
  scale_x_continuous(labels = NULL) +
  coord_flip() 

ggsave("plots/one_way_baseline_sensitivity.jpeg", width = 15)

# Intervention impact 
intervention_names[15] # farm as that is what we are interested in in SEFASI
gp_farm <- gp %>% filter(interven == 15) %>% # H0 same for all interventions
  select(country, name, limit, percH, interven) %>% pivot_wider(names_from = limit, values_from = percH) %>%
  mutate(width = abs(maxval - minval)) %>%
  group_by(country) %>%
  arrange(country, width) %>%
  ungroup() %>%
  mutate(para_number = rep(seq(1,15,1),3)) %>%
  mutate(xmin = para_number - widthval/2,
         xmax = para_number + widthval/2)

g2 <- ggplot(gp_farm) + geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
  theme(axis.text = element_text(size=12),
        axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  facet_wrap( ~ country, scales = "free") +
  scale_fill_manual(breaks =c("LAMBDA_H", "LAMBDA_A", "LAMBDA_E", 
                              "beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E"),
                    values = c("#ABDDA4","#66C2A5","lightgreen", 
                               "red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue")) + 
  scale_x_continuous(labels = NULL) +
  coord_flip() 
ggsave("plots/one_way_farmint_sensitivity.jpeg", width = 15)

g1 / g2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave("plots/figure6.jpeg", width = 10, height = 7)

# Intervention impact - all? 
gp_all <- gp %>% 
  select(country, name, limit, percH, interven) %>% 
  pivot_wider(names_from = limit, values_from = percH) %>%
  mutate(width = abs(maxval - minval)) %>%
  group_by(country, interven) %>%
  arrange(country, interven, width) %>%
  ungroup() %>%
  mutate(para_number = rep(seq(1,15,1),3*length(unique(gp$interven)))) %>%
  mutate(xmin = para_number - widthval/2,
         xmax = para_number + widthval/2) %>%
  mutate(interven_name = intervention_names[interven])

ggplot(gp_all %>% filter(interven %in% seq(12,20,1))) + geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
  theme(axis.text = element_text(size=12),
        axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  facet_grid(country ~ interven_name, scales = "free") +
  scale_fill_manual(breaks =c("LAMBDA_H", "LAMBDA_A", "LAMBDA_E", 
                              "beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E"),
                    values = c("#ABDDA4","#66C2A5","lightgreen", 
                               "red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue")) + 
  scale_x_continuous(labels = NULL) +
  coord_flip() 
ggsave("plots/supp_all_one_way_sensitivity.jpeg", width = 15)


##############################################################################################################
##############################################################################################################
############## Explore how transmission affects impact
intervention_names <- c("H abx to zero","A abx to zero", "E. abx to zero",
                        "HH spread to zero", "AA spread to zero", "EE spread to zero",
                        "A-H spread to zero","EH spread to zero","E-H spread to zero",
                        "E-A spread to zero","EA spread to zero",
                        "Denmark package","England package","Senegal package",
                        "Farm target","H target","E target",
                        "A-H spread down 50%","Spread down 50%","Abx down 50%",
                        "Spread down 30%","Spread down 20%","Spread down 10%")

# how low does the spread parameter have to go before < 100% reduction? 
ggplot(interv_rel %>% filter(para == 320, interven > 20), 
       aes(x=time, y = H, group = interven)) + 
  geom_line(aes(col = factor(interven))) + 
  facet_wrap(~country, scales = "free") + 
  scale_color_discrete(breaks = c(21,22,23), 
                       labels = intervention_names[c(21,22,23)],"Interventions")

ggplot(interv_rel_5yr %>% filter(interven > 20), aes(x=interven, y = diffH, group = interven)) + 
  ggtitle("Difference at 5yrs in humans") + 
  geom_boxplot(aes(fill = factor(interven))) + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_continuous(breaks = c(21,22,23), 
                     labels = intervention_names[c(21,22,23)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("darkslategray","darkslategray","darkslategray", # abx targe
                               "lightgreen","lightgreen","lightgreen", # spread targe
                               "lightgreen","lightgreen","lightgreen",
                               "lightgreen","lightgreen",
                               "chartreuse4","chartreuse4","chartreuse4",# mix
                               "chartreuse4","chartreuse4","chartreuse4",
                               "lightgreen","lightgreen","darkslategray",
                               "lightgreen","lightgreen","lightgreen")) + 
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sens_spread_para_reduction.pdf")
