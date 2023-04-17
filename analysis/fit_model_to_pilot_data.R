library(tidyverse)
library(rstan)
library(patchwork)

options(mc.cores = 8)

# set global ggplot theme
theme_set(ggthemes::theme_tufte())

source("../functions/sim_foraging_data.R")
source("../functions/prep_data.R")
source("../functions/get_run_info.R")


d <- read_csv("../data/polygon pilot Feb 2023/82_foraging_expt_polygons_easy_AB_2023-02-16_17h16.29.457.csv")

######################################################
# check summary stats
######################################################
d %>% modelr::data_grid(person, trial, condition) %>% 
  rename(pp = "person", trl = "trial", blk = "condition") -> d_trls

d_runs <- pmap_df(d_trls, get_run_info, d)

d_runs %>% group_by(person, block) %>%
  summarise(mean_max_run = mean(max_run_length),
            mean_num_run = mean(n_runs),
            .groups = "drop") %>%
  full_join(sim_params) -> d_runs

d_runs %>% select(person, block, mean_max_run, mean_num_run) %>%
  pivot_longer(-c(person, block), names_to = "run_statistic", values_to = "value") %>%
  mutate(block = as_factor(block)) %>%
  ggplot(aes(block, value, fill = run_statistic)) + 
  geom_boxplot() +
  facet_wrap(~run_statistic)

######################################################
# now prepare data for stan model
######################################################

d_stim <- d %>% select(person, block="condition", trial, id, x, y, class) %>%
  arrange(person, block, trial, id) %>%
  filter(class > 0)

d_found <- d %>% filter(found > 0) %>% 
  arrange(person, condition, trial, found) %>%
  rename(block = "condition")

 d_found %>% filter(person == 1, block == 1, trial == 1) %>%
   ggplot(aes(x, y)) + 
   geom_label(aes(label = found, colour = as.factor(class))) + 
   geom_path(data = d_found %>% filter(person == 1, block == 1, trial == 1, found>0),size = 1)

d_list <- prep_data_for_stan(d_found, d_stim)

######################################################
# run model
######################################################

m <- stan("../models/foraging_model_multilevel.stan", data = d_list, 
           chains = 4, iter = 500)

saveRDS(m, "../scratch/sensiv_sim.model")
    
######################################################
# plot model
######################################################
    
source("../functions/plot_model_rr.R")
    
plot_model_fixed(m, d, cond_labels)    

