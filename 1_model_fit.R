### Run model fit to data 

### libraries
library(tidyverse)
library(desolve)
rm(list = ls())
setwd(here())

### Usage
usage <- read.csv("data/input_usage.csv")

### Data to fit to 
res.data <- read.csv("data/res.table.fit.csv")

### 
