#### Usage data
# Have to extend to all timepoints and fill in the gaps in the data 



####### CHECK WHY SOME NAS??? 

### NEED TO REPLICATE FOR MONTHS

library(tidyverse)
theme_set(theme_bw(base_size = 11))
# take data from the literature and get a timeseries for use in ODE solve

usage <- read.csv("data/usage.csv")

# Extend to give data for all years (1900 - 2050)

#usage_full <- 

u <- usage %>% select(country, year, source, kg) %>% 
  complete(country, source, year) %>% 
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

## when were they introduced into each country? for animals and humans?
date_intro <- c(1985, 1985, 1985) 
# min date from data: not by source (use complete later)
minn <- usage %>% filter(kg >0) %>% group_by(country, source) %>% summarise(min = min(year)) 
minn <- rbind(minn, minn[5,])
minn[6,2] <- "animals" # in same year as humans
minn$date_intro <- c(date_intro, date_intro)
minn$min_kg = c(as.numeric(usage %>% filter(country == "denmark", source == "animals", year == minn$min[1]) %>% select("kg")),
                as.numeric(usage %>% filter(country == "denmark", source == "humans",year == minn$min[2]) %>% select("kg")),
                as.numeric(usage %>% filter(country == "england", source == "animals",year == minn$min[3]) %>% select("kg")),
                as.numeric(usage %>% filter(country == "england", source == "humans",year == minn$min[4]) %>% select("kg")),
                as.numeric(usage %>% filter(country == "senegal", source == "humans",year == minn$min[5]) %>% select("kg")*0.27), # use animal approximation
                as.numeric(usage %>% filter(country == "senegal", source == "humans",year == minn$min[6]) %>% select("kg")))
# y = mx + c = grad = m = (y - c)/x
minn$grad = (minn$min_kg-0)/(minn$min - minn$date_intro)

# Fill in 
minn <- minn %>% ungroup()
fill_big <- c()
for(i in c("denmark","senegal","england")){
  for(j in c("animals","humans")){
    
    fill <- c()
    # intro date
    id <- as.numeric(minn %>% filter(country == i, source == j) %>% select(date_intro))
    first_data <- as.numeric(minn %>% filter(country == i, source == j) %>% select(min))
    fill$year = seq(id,first_data,1)
    fill$grad <- as.numeric(minn %>% filter(country == i, source == j) %>% select(grad))
    fill$kg <- fill$grad * (fill$year-as.numeric(minn %>% filter(country == i, source == j) %>% select(date_intro))) + 0
    fill <- as.data.frame(fill)
    fill$country <- i
    fill$source <- j
    
    # Remove completed values in u to stop this repeating (this is a better subtle fit instead of constant backwards)
    w <- intersect(intersect(which(u$country  == i), which(u$source == j)),which(u$year < first_data))
    u[w,"kg"] <- -10 # filter out at end
    
    # Save 
    if(is.null(dim(fill_big))){fill_big <- fill}else{fill_big <- rbind(fill_big, fill)}
  }
}

u_full <- rbind(u %>% filter(kg > -0.001),fill_big[,c(colnames(u))])

ggplot(u_full, aes(x=year, y = kg, group = interaction(country,source))) + 
  geom_line(aes(col = source)) + 
  facet_wrap(~country, scales = "free") + 
  geom_point(data = usage)

## Normalise: divide by max value in the country 
u_full <- u_full %>% group_by(country, source) %>% mutate(max = max(kg),
                                                normalise_kg = kg / max) %>%
  ungroup() %>% 
  group_by(year) %>% 
  complete(country, source) %>% 
  ungroup() %>% 
  dplyr::group_by(country, source) %>%
  fill(normalise_kg, .direction = "downup") %>%
  dplyr::ungroup()

u_full %>% group_by(country, source) %>% summarise(n(), min_yr = min(year), max_yr = max(year)) # Check same number for all 

ggplot(u_full, aes(x=year, y = kg, group = interaction(country,source))) + 
  geom_line(aes(col = source)) + 
  facet_wrap(~country, scales = "free") + 
  geom_point(data = u_full)

# Need to months
u_full <- expand(u_full, year = unique(year), month = 1:12)  %>%
  left_join(u_full, by = 'year') #%>% 
  # filter(year == 1990) %>%
  # print(n=Inf)

intro_date <- min(u_full$year)
u_full <- u_full %>% ungroup()
u_full$norm_year <- (u_full$year - intro_date) 
u_full$time <- (u_full$norm_year * 12) + u_full$month
## normalise_kg needs to change 
u_full$kg_month <- u_full$kg_month/12

ggplot(u_full, aes(x=time, y = normalise_kg, group = interaction(country,source))) + 
  geom_line(aes(col = source)) + 
  facet_wrap(~country, scales = "free") + 
  geom_point(data = u_full)

write.csv(u_full, "data/input_usage.csv")
