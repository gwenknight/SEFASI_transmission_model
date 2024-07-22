### Transmission change over time 

library(tidyverse)
theme_set(theme_bw())

## transmission data
tdata <- read_csv("data/global_food_sefasi.csv") %>%
  pivot_longer(cols = `Production (t)`:`Production per capita (kg)`)

ggplot(tdata, aes(x=Year, y = value, group = Country)) + 
  geom_line(aes(col = Country)) + 
  facet_wrap(~name, scales = "free")
ggsave("plots/sensitivity_transmission_raw_data.jpeg")

### Normalise to peak 
max_vales = tdata %>% group_by(Country) %>% filter(name == "production__tonnes__per_capita") %>%
  summarise(maxx = max(value)) 

tdata_input <- tdata %>% left_join(max_vales) %>% filter(name == "production__tonnes__per_capita") %>%
  mutate(value_norm = value / maxx)

ggplot(tdata_input, aes(x=Year, y = value_norm, group = Country)) + 
  geom_line(aes(col = Country)) + 
  scale_y_continuous("Proxy for transmission normalised to maximum")
ggsave("plots/sensitivity_transmission_normed.jpeg")

### Need weekly
colnames(tdata_input) <- tolower(colnames(tdata_input))
tdata_input <- expand(tdata_input, year = unique(year), week = 1:52)  %>%
  left_join(tdata_input, by = 'year') #%>% 

### Save
write.csv(tdata_input, "data/transmission_ahha.csv")
