#################################################################
#
# This script checks that our simulation code appears to be correct
# and that the features we extract to pass to Stan work as indented
#
#################################################################

library(tidyverse)
library(rstan)
library(patchwork)

options(mc.cores = 8)

# set global ggplot theme
theme_set(theme_bw())

source("../functions/sim_foraging_data.R")
source("../functions/prep_data.R")
source("../functions/get_run_info.R")

######################################################
# first of all we want to generate some synthetic data
######################################################

n_trials_per_cond <- 1

n_targ_class <- 2
n_targ_per_class <- list(c(10, 10))
targ_class_weights <- c(2,1) # A is 3x preferable over B 

# calculate 

bA <- boot::logit(convert_class_weights_to_pA(targ_class_weights))
bS <- 1
sig_d <- 20
sig_theta <- 0



plot_item_selections <- function(d) 
{
  
  d %>% arrange(found) %>%
    mutate(class = as_factor(class)) %>%
    ggplot(aes(x, y, colour = class, label = found)) + 
    geom_point(size = 3) + geom_label() + 
    geom_path(aes( group = 1), colour = "black")
}
  


d <- sim_foraging_person(person = 1,
                                bS, sig_d, sig_theta,
                                block = 1,
                                n_trials_per_cond = 10,
                                n_targ_class = 2, n_targ_per_class, 
                                targ_class_weights)

d_stim <- d %>% select(person, condition, trial, id, x, y, class) %>%
  arrange(person, condition, trial, id) %>%
  filter(class > 0)

d_found <- d %>% filter(found > 0) %>% 
  arrange(person, condition, trial, found)

d_list <- prep_data_for_stan(d_found, d_stim)

m <- stan("../models/foraging_model1_nonml.stan", data = d_list, 
          chains = 4, iter = 1000)



