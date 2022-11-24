library(tidyverse)
library(rstan)
library(patchwork)

options(mc.cores = parallel::detectCores())

# set global ggplot theme
theme_set(ggthemes::theme_tufte())

source("../functions/sim_foraging_data.R")
source("../functions/prep_data.R")
source("../functions/get_run_info.R")

######################################################
# first of all we want to generate some synthetic data
######################################################
cond_labels <- c("equal", "scarce")

n_people <- 10
n_trials_per_cond <- 12

n_targ_class <- 2
n_targ_per_class <- list(c(10, 10), c(5, 15))
targ_class_weights <- list(c(1,1), c(2,1))

b_stick <- 1
sig_d <- 15
sig_theta <- -2


phi_class_weights <- 0.05
phi_stick <- 0.2
phi_d <- 2
phi_theta <- 1

n_conditions <- length(n_targ_per_class)

d <- sim_foraging_people(n_people = n_people,
                         n_conditions = n_conditions,
                         n_trials_per_cond = n_trials_per_cond,
                         n_targ_class = n_targ_class,
                         n_targ_per_class = n_targ_per_class,
                         targ_class_weights = targ_class_weights, phi_class_weights = phi_class_weights,
                         b_stick = b_stick, sig_d = sig_d, sig_theta = sig_theta, # fixed effects
                         phi_stick = phi_stick, phi_d = phi_d, phi_theta = phi_theta) # random effects)
# 
write_csv(d, "../scratch/sim_data.csv")
d <- read_csv("../scratch/sim_data.csv")

d %>% group_by(person, condition) %>%
  summarise(class_weights = unique(class_weights),
    b_stick = unique(b_stick),
            sig_d = unique(sig_d),
            sig_theta = unique(sig_theta)) -> sim_params

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

m <- stan("../models/foraging_model_no_init_sel.stan", data = d_list, 
           chains = 4, iter = 1000)

saveRDS(m, "../scratch/tmp.model")
    
######################################################
# plot model
######################################################
    
source("../functions/plot_model.R")
    
plot_model_fixed(m, d, cond_labels)    

