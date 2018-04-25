

source("www/simcross_model.R")

library(shinyBS)
library(viridis)
library(deldir)
library(tidyverse)
library(plyr)


# Init values

random <- 4
num_cortex <- 6
diam_cortex <- 0.4
size_stele <- 4
diam_stele <- 0.2
proportion_aerenchyma <- 0
n_aerenchyma_files <- 10
n_xylem_files <- 4
diam_xylem <- 0.6
