#### Run univariate sensitivity analysis to explore sensitivity of output to parameters####
################### Gwen Knight ########################################################
################################## May 2024 #####################################

rm(list = ls())
library(tidyverse)
library(patchwork)
library(here)
setwd(here())
theme_set(theme_bw())
# initial values
source("0_initial_conditions.R")

intervention_names <- c("H abx to zero","A abx to zero", "E. abx to zero",
                        "HH spread to zero", "AA spread to zero", "EE spread to zero",
                        "A-H spread to zero","EH spread to zero","E-H spread to zero",
                        "E-A spread to zero","EA spread to zero",
                        "Denmark package","England package","Senegal package",
                        "Farm target","H target","E target",
                        "A-H spread down 50%","Spread down 50%","Abx down 50%",
                        "Spread down 30%","Spread down 20%","Spread down 10%", "Abx animals down 30%")

# intervention impact 
interv_rel <- read.csv("output/interv_rel.csv")[,-1]
interv_rel_5yr <- interv_rel %>% filter(time == 260) %>% 
  rowwise() %>% 
  mutate(percH = 100 * diffH/H0,
         percA = 100 * diffA/A0,
         percE = 100 * diffE/E0) 

interv_rel_5yr_24 <- interv_rel_5yr %>% filter(interven == 24) %>%
  group_by(country) %>%
  summarise(mean = median(percH),
            min025 = quantile(percH,probs=c(0.025)), 
            max975 = quantile(percH,probs=c(0.975)))

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

ggplot(gp, aes(x=para, y = H0)) + geom_point()

# Baseline analysis 
widthval = 0.8
gp_baseline <- gp %>% filter(interven == 1) %>% # H0 same for all interventions
  select(country, name, limit, H0, interven) %>% 
  pivot_wider(names_from = limit, values_from = H0) %>%
  mutate(width = abs(maxval - minval)) %>%
  group_by(country) %>%
  arrange(country, width) %>%
  ungroup() %>%
  mutate(para_number = rep(seq(1,15,1),3)) %>%
  mutate(xmin = para_number - widthval/2,
         xmax = para_number + widthval/2)

### Mean => to plot on this => average over the final year 
best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1]
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1]
best_100_england <- read_csv("output/best_100_england.csv")[,-1]
best <- rbind(best_100_senegal %>% mutate(ctry = "Senegal", year = rep(c(init_senegal_year,rep(seq(init_senegal_year+1,2022,1),each = 52)),100)),
              best_100_england %>% mutate(ctry ="England", year = rep(c(init_england_year,rep(seq(init_england_year+1,2022,1),each = 52)),100)),
              best_100_denmark %>% mutate(ctry ="Denmark", year = rep(c(init_denmark_year,rep(seq(init_denmark_year+1,2022,1),each = 52)),100))) %>%
  pivot_longer(cols = c("H","A","E"))

### Averages 
fits_av <- best %>% group_by(year, ctry, name) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975))) %>% 
  filter(year == 2022, name == "H") %>%
  rename(country = ctry)

g1 <- ggplot(gp_baseline) + 
  geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
  theme(axis.text = element_text(size=12),
        axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_fill_manual(breaks =c("beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E",
                              "LAMBDA_H", "LAMBDA_A", "LAMBDA_E"),
                    values = c("red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue",
                               "#ABDDA4","#66C2A5","lightgreen"),
                    labels = c("beta_HH"= expression(beta[HH]), "beta_AA"= expression(beta[AA]), 
                               "beta_EE"= expression(beta[EE]), "beta_AH"= expression(beta[AH]), 
                               "beta_HA"= expression(beta[HA]), "beta_EH"= expression(beta[EH]), 
                               "beta_EA"= expression(beta[EA]), "beta_AE"= expression(beta[AE]), 
                               "beta_HE"= expression(beta[HE]), 
                               "mu_H"= expression(mu[H]), "mu_A"= expression(mu[A]), "mu_E"= expression(mu[E]),
                    "LAMBDA_H" = expression(lambda[H]),
                    "LAMBDA_A"= expression(lambda[A]), "LAMBDA_E"= expression(lambda[E]))) + 
  geom_point(data = fits_av, aes(x=0, y = mean)) + 
  geom_errorbar(data = fits_av, aes(x = 0, ymin = min025, ymax = max975)) + 
  facet_wrap( ~ country) + 
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("") + 
  coord_flip() 

ggsave("plots/one_way_baseline_sensitivity.jpeg", width = 15)




# Intervention impact 
intervention_names[24] # AMU in animals for SEFASI
gp_farm <- gp %>% filter(interven == 24) %>% # H0 same for all interventions
  select(country, name, limit, percH, interven) %>% 
  pivot_wider(names_from = limit, values_from = percH) %>%
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
  scale_fill_manual(breaks =c("beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E",
                              "LAMBDA_H", "LAMBDA_A", "LAMBDA_E"),
                    values = c("red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue",
                               "#ABDDA4","#66C2A5","lightgreen"),
                    labels = c("beta_HH"= expression(beta[HH]), "beta_AA"= expression(beta[AA]), 
                               "beta_EE"= expression(beta[EE]), "beta_AH"= expression(beta[AH]), 
                               "beta_HA"= expression(beta[HA]), "beta_EH"= expression(beta[EH]), 
                               "beta_EA"= expression(beta[EA]), "beta_AE"= expression(beta[AE]), 
                               "beta_HE"= expression(beta[HE]), 
                               "mu_H"= expression(mu[H]), "mu_A"= expression(mu[A]), "mu_E"= expression(mu[E]),
                    "LAMBDA_H" = expression(lambda[H]),
                    "LAMBDA_A"= expression(lambda[A]), "LAMBDA_E"= expression(lambda[E]))) + 
  geom_point(data = interv_rel_5yr_24, aes(x=0, y = mean)) + 
  geom_errorbar(data = interv_rel_5yr_24, aes(x = 0, ymin = min025, ymax = max975)) + 
  facet_wrap( ~ country) +
  coord_flip() + 
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("")
ggsave("plots/one_way_farmint_sensitivity.jpeg", width = 15)

g1 / g2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave("plots/figure6.jpeg", width = 10, height = 7)

###### 5th / 95th parameter? 
# Of those 100 best parameters, how much is intervention affected? 
# Instead of max and min, extract 5th and 95th parameter 
best_para <- read.csv("output/best_para.csv")[,-1]

para_range_max <- best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E") %>%
  group_by(ctry, name) %>%
  summarise(maxval = nth(value,95, order_by = value),
            para = ifelse(value == maxval,para,0)) %>%
  filter(para > 0) %>%
  pivot_longer(col = maxval, names_to = "limit", values_to = "value") %>% 
  rename(country = ctry)

para_range_min <- best_para %>% pivot_longer(cols = "LAMBDA_H":"mu_E") %>%
  group_by(ctry, name) %>%
  summarise(minval = nth(value,5, order_by = value),
            para = ifelse(value == minval,para,0)) %>%
  filter(para > 0) %>% 
  pivot_longer(col = minval, names_to = "limit", values_to = "value") %>% 
  rename(country = ctry)

pp <- rbind(para_range_max, para_range_min)

gp <- left_join(pp, interv_rel_5yr[,c("country","para","interven","H0","percH")]) %>% 
  group_by(country, interven, limit) %>%
  arrange(H0)

ggplot(gp, aes(x=para, y = H0)) + geom_point()

# Baseline analysis 
widthval = 0.8
gp_baseline <- gp %>% filter(interven == 1) %>% # H0 same for all interventions
  select(country, name, limit, H0, interven) %>% 
  pivot_wider(names_from = limit, values_from = H0) %>%
  mutate(width = abs(maxval - minval)) %>%
  group_by(country) %>%
  arrange(country, width) %>%
  ungroup() %>%
  mutate(para_number = rep(seq(1,15,1),3)) %>%
  mutate(xmin = para_number - widthval/2,
         xmax = para_number + widthval/2)

### Mean => to plot on this => average over the final year 
best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1]
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1]
best_100_england <- read_csv("output/best_100_england.csv")[,-1]
best <- rbind(best_100_senegal %>% mutate(ctry = "Senegal", year = rep(c(init_senegal_year,rep(seq(init_senegal_year+1,2022,1),each = 52)),100)),
              best_100_england %>% mutate(ctry ="England", year = rep(c(init_england_year,rep(seq(init_england_year+1,2022,1),each = 52)),100)),
              best_100_denmark %>% mutate(ctry ="Denmark", year = rep(c(init_denmark_year,rep(seq(init_denmark_year+1,2022,1),each = 52)),100))) %>%
  pivot_longer(cols = c("H","A","E"))

### Averages 
fits_av <- best %>% group_by(year, ctry, name) %>%
  summarise(mean = mean(value),
            min025 = quantile(value,probs=c(0.025)), 
            max975 = quantile(value,probs=c(0.975))) %>% 
  filter(year == 2022, name == "H") %>%
  rename(country = ctry)

g1 <- ggplot(gp_baseline) + 
  geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
  theme(axis.text = element_text(size=12),
        axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_fill_manual(breaks =c("beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E",
                              "LAMBDA_H", "LAMBDA_A", "LAMBDA_E"),
                    values = c("red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue",
                               "#ABDDA4","#66C2A5","lightgreen"),
                    labels = c("beta_HH"= expression(beta[HH]), "beta_AA"= expression(beta[AA]), 
                               "beta_EE"= expression(beta[EE]), "beta_AH"= expression(beta[AH]), 
                               "beta_HA"= expression(beta[HA]), "beta_EH"= expression(beta[EH]), 
                               "beta_EA"= expression(beta[EA]), "beta_AE"= expression(beta[AE]), 
                               "beta_HE"= expression(beta[HE]), 
                               "mu_H"= expression(mu[H]), "mu_A"= expression(mu[A]), "mu_E"= expression(mu[E]),
                               "LAMBDA_H" = expression(lambda[H]),
                               "LAMBDA_A"= expression(lambda[A]), "LAMBDA_E"= expression(lambda[E]))) + 
  geom_point(data = fits_av, aes(x=0, y = mean)) + 
  geom_errorbar(data = fits_av, aes(x = 0, ymin = min025, ymax = max975)) + 
  facet_wrap( ~ country) + 
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("") + 
  coord_flip() 

# Intervention impact 
intervention_names[24] # AMU in animals for SEFASI
gp_farm <- gp %>% filter(interven == 24) %>% # H0 same for all interventions
  select(country, name, limit, percH, interven) %>% 
  pivot_wider(names_from = limit, values_from = percH) %>%
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
  scale_fill_manual(breaks =c("beta_HH", "beta_AA", "beta_EE", "beta_AH", "beta_HA", 
                              "beta_EH", "beta_EA", "beta_AE", "beta_HE", 
                              "mu_H", "mu_A", "mu_E",
                              "LAMBDA_H", "LAMBDA_A", "LAMBDA_E"),
                    values = c("red","#9E0142","#D53E4F","#F46D43","#FDAE61","gold","#FEE08B","#FFFFBF","#E6F598",  
                               "#3288BD","#5E4FA2","mediumblue",
                               "#ABDDA4","#66C2A5","lightgreen"),
                    labels = c("beta_HH"= expression(beta[HH]), "beta_AA"= expression(beta[AA]), 
                               "beta_EE"= expression(beta[EE]), "beta_AH"= expression(beta[AH]), 
                               "beta_HA"= expression(beta[HA]), "beta_EH"= expression(beta[EH]), 
                               "beta_EA"= expression(beta[EA]), "beta_AE"= expression(beta[AE]), 
                               "beta_HE"= expression(beta[HE]), 
                               "mu_H"= expression(mu[H]), "mu_A"= expression(mu[A]), "mu_E"= expression(mu[E]),
                               "LAMBDA_H" = expression(lambda[H]),
                               "LAMBDA_A"= expression(lambda[A]), "LAMBDA_E"= expression(lambda[E]))) + 
  geom_point(data = interv_rel_5yr_24, aes(x=0, y = mean)) + 
  geom_errorbar(data = interv_rel_5yr_24, aes(x = 0, ymin = min025, ymax = max975)) + 
  facet_wrap( ~ country) +
  coord_flip() + 
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("")

g1 / g2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave("plots/5th_95th_figure6.jpeg", width = 10, height = 7)

###################

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

ggplot(gp_all %>% filter(interven %in% c(seq(12,20,1),24))) + 
  geom_rect(aes(ymax = maxval, ymin = minval, xmin = xmin, xmax = xmax, fill = name)) + 
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

ggplot(interv_rel_5yr %>% filter(interven > 20, interven < 24), aes(x=interven, y = diffH, group = interven)) + 
  ggtitle("Difference at 5yrs in humans") + 
  geom_boxplot(aes(fill = factor(interven))) + 
  facet_wrap(~country,ncol = 1) + 
  scale_x_continuous(breaks = c(21,22,23), 
                     labels = intervention_names[c(21,22,23)],"Interventions") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(breaks = seq(21,23,1), values = c("lightgreen","lightgreen","lightgreen","lightgreen")) + 
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "none") + 
  scale_y_continuous("Absolute difference in proportion resistant at 5yrs")
ggsave("plots/sens_spread_para_reduction.jpeg")
