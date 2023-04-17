library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())
source("../functions/prep_data.R")


folder <- "../data/polygon pilot feature conjunction/"


d_found <- read_csv(paste0("../output/d_found.csv")) %>% 
  rename(person = "participant") %>%
  unite(condition, difficulty, common)

d_stim <- read_csv(paste0("../output/d_stim.csv")) %>% 
  rename(person = "participant") %>%
  unite(condition, difficulty, common)

d_list <- prep_data_for_stan(d_found, d_stim) 
d_list$prior_mu_phidis <- 10

#d_list$targ_class <- d_list$targ_class-1

m <- stan("../../foraging_spatial/models/foraging_model1.stan", data = d_list,
          chains = 1, iter = 1000)

saveRDS(m, "foraging_pilot.model")




blks_labels <- levels(d_found$condition)

source("../functions/plot_model.R")


plot_model_fixed(m, d_found, blks_labels)