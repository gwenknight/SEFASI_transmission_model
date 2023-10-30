
#from Knight paper
#Both acquisition rates (via transmission and de novo emergence) were dependent on 
#exposure to antibiotics, as antibiotic use clears sensitive bacterial carriage, 
#predisposing a host to colonisation with the (new) ARB. Linking transmission directly 
#to antibiotic exposure captures this impact of selection on both the source of 
#transmission (antibiotic exposure increases the ARB load) and the receiver 
require(deSolve)
require(ggplot2)
require(lhs)
#require(IMIS)
require(dplyr) 
require(reshape)
require(pbapply)
require(gridExtra)
require(wesanderson)
require(cowplot)
require(gmp)
require(data.table)
require(grid)
require(gtable)
library(bayestestR)
library(magick)
library(RColorBrewer)
library(yarrr)
library(parallel)
library(compiler)
library(purrr)
library(profvis)
library(furrr)