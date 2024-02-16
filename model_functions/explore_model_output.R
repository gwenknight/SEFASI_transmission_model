###################################################################################
################################    SEFASI transmission model explore output    #####################################
################################# Ross Booton & Gwen Knight #####################################
##################################  Oct 2023  #####################################
###################################################################################

rm(list = ls())
source("model_functions/packages.R")
setwd(here())
theme_set(theme_bw())

################################  MODEL OUTPUT #####################################
###### Read in model runs on 100,000 parameter inputs
# FULLDATA_DENMARK <-fread("output/OUT_denmark.csv") # old from outfun
# FULLDATA_ENGLAND <-fread("output/OUT_england.csv")
# FULLDATA_SENEGAL <-fread("output/OUT_senegal.csv") 

# new from cluster run 
FULLDATA_DENMARK <-fread("fits/denmark100000.csv")
FULLDATA_ENGLAND <-fread("fits/england100000.csv")
FULLDATA_SENEGAL <-fread("fits/senegal100000.csv")


FULLDATA <- rbind(FULLDATA_DENMARK %>% mutate(ctry = "denmark", runs = seq(1, dim(FULLDATA_DENMARK)[1])), 
                  FULLDATA_ENGLAND %>% mutate(ctry = "england", runs = seq(1, dim(FULLDATA_ENGLAND)[1])), 
                  FULLDATA_SENEGAL %>% mutate(ctry = "senegal", runs = seq(1, dim(FULLDATA_SENEGAL)[1])))

################################ EXPLORE ###########################################
FULLDATA2 <- FULLDATA %>% pivot_longer(cols = model2000.H:model2021.E) 
FULLDATA2$year2 <- sub('.*model', '', FULLDATA2$name)
FULLDATA2$year <- str_split_fixed(FULLDATA2$year2, fixed("."), 2)[, 1]
FULLDATA2$env <- str_split_fixed(FULLDATA2$year2, fixed("."), 2)[, 2]
FULLDATA <- FULLDATA2 %>% select(-c(year2, name))

# ggplot(FULLDATA, aes(x=year, y = value, group = runs)) + geom_line() + 
#   facet_grid(ctry ~ env)

ggplot(FULLDATA %>% filter(runs < 5), aes(x=year, y = value, group = runs)) + geom_line(aes(col = factor(runs))) + 
  facet_grid(ctry ~ env)


FULLDATA %>% filter(ctry == "denmark", year == 2021, env == "H") %>% filter(value < 0.2)

ggplot(FULLDATA %>% filter(runs %in% c(136, 218, 308, 820)), aes(x=year, y = value, group = ctry)) + geom_line(aes(col = factor(ctry))) + 
  facet_grid(runs ~ env)

########################################################################################
########################################################################################
########################################################################################
################################  FITTING DATA #####################################
### Resistance prevalence data cleaned 
#source("model_functions/res_prev_band_data.R")

#### Antibiotic usage
#usage.table <- as.data.frame(read.csv("data/usage.csv"))

#### Plot this input data - only do first time
# source("plotinputdata.R")
# #### How far can the model be from the data? 
# MARGIN_TOLERANCE = 0.10
# # Adapts the resistance prevalence data 
# source("model_functions/total_max_min_example.R") # For each year, take the max / min of the data for resistance prevalence +/- margin of tolerance. Ignore warnings() - to do with years that have no data
# # -> Generates "sup_inf_data"

# #source("how.many.fit.R")
# 
# #FITS_DENMARK <- how.many.fit(FULLDATA_orig,"denmark",0.025)
# #nrow(FITS_DENMARK)
# #FITS_ENGLAND <- how.many.fit(FULLDATA_orig,"england",0.035)
# #nrow(FITS_ENGLAND)
# #FITS_SENEGAL <- how.many.fit(FULLDATA_orig,"senegal",0.13)
# #nrow(FITS_SENEGAL)
# 
# #FULLDATA <- FITS_DENMARK

## Load function to calculate log likelihood 
source("model_functions/LL_function.R")

### Explore distance from data for each country 
MLE_SENEGAL <- rep(NA,nrow(FULLDATA_SENEGAL) )
MLE_ENGLAND <- rep(NA,nrow(FULLDATA_ENGLAND) )
MLE_DENMARK <- rep(NA,nrow(FULLDATA_DENMARK) )

#### Data with one data point per time point
res.table.fit <- read_csv("data/res.table.fit.csv") %>% mutate(prop = percent / 100)
ggplot(res.table.fit, aes(x=time, y = percent, group = country)) + geom_line(aes(col = country)) + facet_wrap(~var) + geom_point(aes(col = country))


for(i in 1:nrow(FULLDATA_SENEGAL)) {
  MLE_SENEGAL[i] =  LL_simple(FULLDATA_SENEGAL[i,], "senegal", res.table.fit)
  #print(i)
}

for(i in 1:nrow(FULLDATA_DENMARK)) {
  MLE_DENMARK[i] =  LL_simple(FULLDATA_DENMARK[i,], "denmark", res.table.fit)
 # print(i)
}

for(i in 1:nrow(FULLDATA_ENGLAND)) {
  MLE_ENGLAND[i] =  LL_simple(FULLDATA_ENGLAND[i,],"england", res.table.fit)
#  print(i)
}

# Store log-likelihood values 
write.csv(MLE_SENEGAL,"output/MLE_SENEGAL.csv")
write.csv(MLE_DENMARK,"output/MLE_DENMARK.csv")
write.csv(MLE_ENGLAND,"output/MLE_ENGLAND.csv")

# Find the max 100 and store
MLE_SENEGAL[order(-MLE_SENEGAL)[1:100]]
best_100_senegal <- FULLDATA_SENEGAL[order(-MLE_SENEGAL)[1:100],]
best_100_england <- FULLDATA_ENGLAND[order(-MLE_ENGLAND)[1:100],]
best_100_denmark <- FULLDATA_DENMARK[order(-MLE_DENMARK)[1:100],]


write.csv(best_100_senegal,"output/best_100_senegal.csv")
write.csv(best_100_england,"output/best_100_england.csv")
write.csv(best_100_denmark,"output/best_100_denmark.csv")

########################################################################################
########################################################################################
########################################################################################
############################################ Visualise fits ############################################
#### Read in what need
res.table.fit <- read_csv("data/res.table.fit.csv") %>% mutate(prop = percent / 100)

best_100_senegal <- read_csv("output/best_100_senegal.csv")[,-1]
best_100_denmark <- read_csv("output/best_100_denmark.csv")[,-1]
best_100_england <- read_csv("output/best_100_england.csv")[,-1]

best <- rbind(best_100_senegal %>% mutate(country = "senegal"),
              best_100_england %>% mutate(country ="england"),
              best_100_denmark %>% mutate(country ="denmark"))
# rename
c <- colnames(best)
c[1] <- "run"
c[68] <- "country"
colnames(best) <- c

# pivot around
model_output <- best %>% 
  pivot_longer("model2000.H":"model2021.E") %>% 
  mutate(environment = sub(".*\\.", "", name), 
         year = as.numeric(substring(str_extract(name, "^[^\\.:]+"), nchar(str_extract(name, "^[^\\.:]+"))-3)))

data_fit <- res.table.fit %>% mutate(environment = var)

ggplot(model_output, aes(x=year, y = value, group = run)) + geom_line() + 
  geom_point(data = data_fit,aes(x=time, y = prop, group = country), col = "red") + 
  facet_wrap(country~environment,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits.pdf")

ggplot(model_output, aes(x=year, y = value, group = run)) + geom_line() + 
  geom_point(data = data_fit,aes(x=time, y = prop, group = country), col = "red") + 
  facet_wrap(environment~country,ncol = 3) + 
  scale_y_continuous("Proportion resistant") + 
  scale_x_continuous("Year")

ggsave("plots/model_fits_flip.pdf")

############# Visualise parameter outputs
p1 <- read.csv("output/parameter_set_100000.csv")

runs <- best$run
length(unique(runs)) # not the same for the countries => 300 unique 

write_csv(cbind(p1[runs,], "country" = best$country), "output/best_parameter_sets.csv")

#### Visualise parameters
fit_para <- cbind(p1[runs,], best$country) %>% pivot_longer(cols = LAMBDA_H:epsilon)
colnames(fit_para) <- c("pset","country","para","value")
ggplot(fit_para, aes(x=para, y = value)) + geom_violin() + facet_wrap(~country, ncol = 1)
ggplot(fit_para, aes(x=para, y = value)) + geom_boxplot() + facet_wrap(~country, ncol = 1)


###############

### Load functions to run interventions 
source("model_functions/AMRmodel.R")
source("model_functions/epid.R")
source("plot_functions/explore_and_plot_time_varying_usage.R")
source("model_functions/epid_intervention.R")
source("plot_functions/plotfits_int.R")

sup_inf_data <- read.csv("output/sup_inf_data")
plotfits_int(best_100_england,"england",0, sup_inf_data)


################################ boxplot of interventions  ################################ 
getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
cols <- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
          unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white","black")

source("plotfits_int_box.R")

plotfits_int_box(best_100_england,"england")
plotfits_int_box(best_100_denmark,"denmark")
plotfits_int_box(best_100_senegal,"senegal")

source("combined_plots.R") #produces the plots by interventiona and country




