### Initial conditions

### libraries
library(tidyverse)
library(here)
rm(list = ls())
setwd(here())

## Functions
source("0_model_functions.R")

### Usage
usage <- read.csv("data/input_usage.csv")

### Data to fit to 
res.data <- read.csv("data/res.table.fit.csv")
ggplot(res.data, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))
##### What initial conditions? 
# England
res.data %>% filter(country == "england") %>% select(time, percent, var) %>%
  group_by(var) %>% 
  mutate(mint = min(time),
         value_min = ifelse(time == mint, percent, -100)) %>%
  filter(value_min > -100)
# 2001 at 1.2 for resistance in H: others all at zero
init_england <- c(0.012, 0, 0)
init_england_year <- 2001

# Denmark
res.data %>% filter(country == "denmark") %>% select(time, percent, var) %>%
  group_by(var) %>% 
  mutate(mint = min(time),
         value_min = ifelse(time == mint, percent, -100)) %>%
  filter(value_min > -100)
# 2005 at 1.15 for resistance in H: others all at zero
init_denmark <- c(0.0115, 0, 0)
init_denmark_year <- 2005

# Senegal
res.data %>% filter(country == "senegal") %>% select(time, percent, var) %>%
  group_by(var) %>% 
  mutate(mint = min(time),
         value_min = ifelse(time == mint, percent, -100)) %>%
  filter(value_min > -100)
# 2003 at 8.96% for resistance in H: animals high at 24% in 2006, but 10% later. E later timepoint. 
init_senegal <- c(0.0896, 0.1, 0)
init_senegal_year <- 2003
