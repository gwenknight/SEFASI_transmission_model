############# Usage data ######################################################
######### Gwen Knight based on initial work by Ross Booton ####################
########## May 2024 ###########################################################

# Have to extend to all timepoints and fill in the gaps in the data 

library(tidyverse)
theme_set(theme_bw(base_size = 11))

source("0_initial_conditions.R")
min_year <- min(init_denmark_year, init_england_year, init_senegal_year)

# Data from literature: fill in gaps for model 
usage <- read.csv("data/usage.csv")

# Extend to give data for all years from start of 1900s for now

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
# 0.279 / 0.263 for denmark and england => but these are outliers
# globally 70% in animals, 30% in humans => ratio should be 2.3


senegal_animal_approx <- u %>% filter(country == "senegal", source == "humans") %>%
  mutate(kg = kg * 2.3) # OLD: 0.27) 
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
    w <- intersect(intersect(which(u$country  == i), which(u$source == j)),which(u$year <= first_data))
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

## Denmark: antibiotic usage controls from 2010. 
# Only data in 2003 and 2015
usage %>% filter(country == "denmark", source == "animals")
ggplot(u_full %>% filter(country == "denmark", source == "animals"), aes(x=year, y = kg)) + geom_line()
# Fill in same level 2003 - 2010
# then decline 2010 to 2015 value
d_10 <- u_full %>% filter(country == "denmark", source == "animals", year == 2010)
d_15 <- u_full %>% filter(country == "denmark", source == "animals", year == 2015)

grad_1015 <- (d_15$kg - d_10$kg)/5

w11 <- intersect(intersect(which(u_full$country == "denmark"), which(u_full$source == "animals")), which(u_full$year == 2011))
w12 <- intersect(intersect(which(u_full$country == "denmark"), which(u_full$source == "animals")), which(u_full$year == 2012))
w13 <- intersect(intersect(which(u_full$country == "denmark"), which(u_full$source == "animals")), which(u_full$year == 2013))
w14 <- intersect(intersect(which(u_full$country == "denmark"), which(u_full$source == "animals")), which(u_full$year == 2014))
u_full[w11,"kg"] <- d_10$kg + grad_1015 * 1 
u_full[w12,"kg"] <- d_10$kg + grad_1015 * 2 
u_full[w13,"kg"] <- d_10$kg + grad_1015 * 3 
u_full[w14,"kg"] <- d_10$kg + grad_1015 * 4 
# have a decline now from 2010 
ggplot(u_full %>% filter(country == "denmark", source == "animals"), aes(x=year, y = kg)) + geom_line()


### Environment? 
u_full <- u_full %>%
  pivot_wider(names_from = source, values_from = kg) %>%
  mutate(environ = (animals + humans)) %>% # relative to sum 
  pivot_longer(cols = c("animals","humans","environ"), values_to = "kg", names_to = "source")

## Normalise: divide by max value in the country 
u_full <- u_full %>% group_by(country, source) %>% 
  mutate(max = max(kg), normalise_kg = kg / max) %>%
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

# Need to weekly
u_full <- expand(u_full, year = unique(year), week = 1:52)  %>%
  left_join(u_full, by = 'year') #%>% 
# filter(year == 1990) %>%
# print(n=Inf)

intro_date <- min(u_full$year)
u_full <- u_full %>% ungroup()
u_full$norm_year <- (u_full$year - intro_date) 
u_full$time <- (u_full$norm_year * 52) + u_full$week

ggplot(u_full, aes(x=year, y = normalise_kg, group = interaction(country,source))) +
  geom_line(aes(col = source)) +
  facet_wrap(~country, scales = "free") +
  geom_point(data = u_full) + 
  geom_hline(yintercept = 1) #+ 
#scale_y_continuous(limits = c(0.95,1))

write.csv(u_full, "data/input_usage.csv") # with environment! 

#### Figure for paper 
ggplot(u_full, aes(x=year, y = kg, group = interaction(country,source))) +
  geom_line(aes(col = source)) +
  facet_wrap(~country, scales = "free") + 
  scale_x_continuous(limits = c(min_year, 2025))

u_full[which(u_full$country == "denmark"),"country"] <- "Denmark"
u_full[which(u_full$country == "senegal"),"country"] <- "Senegal"
u_full[which(u_full$country == "england"),"country"] <- "England"

intro <- as.data.frame(matrix(0,3,2))
colnames(intro) <- c("country","intro_date")
intro$country<- c("Denmark","England","Senegal")
intro$intro_date <- c(2005,2001,2003)

ggplot(u_full, aes(x=year, y = normalise_kg, group = interaction(country,source))) +
  geom_line(aes(col = source)) +
  facet_wrap(~country, scales = "free") + 
  scale_x_continuous(limits = c(min_year, 2025),"Year") + 
  scale_color_manual("Setting", breaks = c("animals","environ", "humans"),values = c("brown3","cornflowerblue","darkgoldenrod1"), labels = c("Animals", "Environment","Humans")) + 
  scale_y_continuous("Normalised level (usage_x)") + 
  geom_vline(data = intro, aes(xintercept = intro_date), lty = "dashed") + 
  theme(legend.position="bottom")
ggsave("plots/usage.jpg", width = 10, height = 10)

