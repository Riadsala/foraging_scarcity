library(tidyverse)
library(rstan)
library(patchwork)



options(mc.cores = parallel::detectCores())

# set global ggplot theme
theme_set(ggthemes::theme_solarized_2())

source("../functions/sim_foraging_data.R")
source("../functions/prep_data.R")
source("../functions/get_run_info.R")

######################################################
# first of all we want to generate some synthetic data
######################################################
cond_labels <- c("equal", "equal-bias", "scarce", "scarce-bias")


m <- readRDS("../scratch/tmp.model")

######################################################
# plot model
######################################################

source("../functions/plot_model.R")

plot_model_fixed(m, d, cond_labels)    
