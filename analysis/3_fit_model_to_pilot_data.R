library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())

# set ggplot2 theme
theme_set(theme_bw())

source("../functions/prep_data.R")

experiment <- "polygon pilot feature conjunction"

d_found <- read_csv(paste0("../output/", experiment, "/d_found.csv")) %>% 
  unite(condition, difficulty, common)

d_stim <- read_csv(paste0("../output/", experiment, "/d_stim.csv")) %>% 
  unite(condition, difficulty, common)

d_list <- prep_data_for_stan(d_found, d_stim) 
d_list$prior_mu_phidis <- 10

mod <- cmdstan_model("../models/foraging_model1.stan")

m <- mod$sample(data = d_list, chains = 4, parallel_chains = 4)
saveRDS(m, (paste0("../output/", experiment, "_foraging_pilot.model"))


source("../functions/plot_model.R")


plot_model_fixed(m, d_found, merge_conditions=TRUE, fix_priorNames = TRUE)

