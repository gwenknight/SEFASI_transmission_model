#### Usage data
# Have to extend to all timepoints and fill in the gaps in the data 

library(tidyverse)
theme_set(theme_bw(base_size = 11))
# take data from the literature and get a timeseries for use in ODE solve

usage <- read.csv("data/usage.csv")

# Extend to give data for all years (1900 - 2050)

#usage_full <- 

u <- usage %>% select(country, year, source, kg) %>% 
  complete(country, year, source) %>% 
  ungroup() %>% 
  dplyr::group_by(country, source) %>%
  fill(kg, .direction = "downup") %>%
  dplyr::ungroup()

ggplot(u, aes(x=year, y = kg, group = interaction(country,source))) + 
  geom_line(aes(col = country)) + 
  facet_wrap(~source, scales = "free") + 
  geom_point(data = usage)

ggplot(u, aes(x=year, y = kg, group = interaction(country,source))) + 
  geom_line(aes(col = source)) + 
  facet_wrap(~country, scales = "free") + 
  geom_point(data = usage)

# What is the ratio of human to animal use? 
ratio <- usage %>% select(country, year, source, kg) %>% 
  group_by(country) %>% pivot_wider(names_from = source, values_from = kg) %>%
  mutate(ratio = animals / humans) %>%
  summarise(mean(ratio, na.rm = TRUE))
# 0.279 / 0.263 for denmark and england

senegal_animal_approx <- u %>% filter(country == "senegal", source == "humans") %>%
  mutate(kg = kg * 0.27) 
senegal_animal_approx$source <- "animals"

# Add to other data 
w <- intersect(which(u$source == "animals"), which(u$country == "senegal"))
u <- rbind(u[-w,], senegal_animal_approx)

ggplot(u, aes(x=year, y = kg, group = interaction(country,source))) + 
  geom_line(aes(col = source)) + 
  facet_wrap(~country, scales = "free") + 
  geom_point(data = usage)

write.csv(u, "data/input_usage.csv")
