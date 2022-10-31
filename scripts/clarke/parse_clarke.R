library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
library(scales)

source("../../functions/sim_foraging_data.R")
source("../../functions/prep_data.R")
source("../../functions/get_run_info.R")


options(mc.cores = parallel::detectCores())

dc <- read_csv("../../data/clarke2020/clarke_2020_qjep.csv") %>%
  select(person = "observer", block = "condition", trial = "trial",  
         id = "id", found = "found", class = "targ_type",
         x = "x", y = "y") 

# for testing
#write_csv(dc, 'subset_clarke.csv')
dc <- read_csv('subset_clarke.csv')

d_stim <- dc %>% select(person, block, trial, id, x, y, class) %>%
  arrange(person, block, trial) 

d_found <- dc %>% filter(found > 0) %>% 
  arrange(person, block, trial, found) 



d_list <- prep_data_for_stan(d_found, d_stim)

#saveRDS(d_list, 'clarke_stan.rds')

m <- stan("../../models/foraging_model_no_init_sel.stan", data = d_list, 
          chains = 4, iter = 1000)
summary(m)

saveRDS(m, 'clarke_model.rds')

# plotting
source("../../functions/plot_model.R")

library(ggdark)

theme_set(dark_theme_bw())

plot_model_fixed(m, dc)
plot_model_spatial(m, dc)
plot_init_sel(m, dc)

