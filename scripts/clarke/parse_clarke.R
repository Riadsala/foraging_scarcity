library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
library(scales)

source("../../functions/sim_foraging_data.R")
source("../../functions/prep_data.R")
source("../../functions/get_run_info.R")


options(mc.cores = parallel::detectCores())

# it also gets stuck if people have different numbers of trials

dc <- read_csv("../../data/clarke2020/clarke_2020_qjep.csv") %>%
  select(person = "observer", block = "condition", trial = "trial",  
         id = "id", found = "found", class = "targ_type",
         x = "x", y = "y") %>%
  filter(trial < 6) %>%
  filter(person < 6)

write_csv(dc, 'subset_clarke.csv')
dc <- read_csv('subset_clarke.csv')

dc %>% mutate(x = as.vector(rescale(x, to = c(0.01, 0.99))),
              y = as.vector(rescale(y, to = c(0.01, 0.99)))) -> dc 

dc_summary <- dc %>%
  group_by(person, block) %>%
  summarise(n = n())

d_stim <- dc %>% select(person, block, trial, id, x, y, class) %>%
  arrange(person, block, trial) 

d_found <- dc %>% filter(found > 0) %>% 
  arrange(person, block, trial, found) 


d_found %>% filter(block == 1, trial == 2, person == 1) %>%
  ggplot(aes(x, y)) + 
  geom_label(aes(label = id, colour = as.factor(class))) + 
  geom_path(data = d_found %>% filter(block == 1, trial == 2, person == 1, found>0),size = 1)

d_list <- prep_data_for_stan(d_found, d_stim)

saveRDS(d_list, 'clarke_stan.rds')

m <- stan("../../models/foraging_model_multilevel.stan", data = d_list, 
          chains = 4, iter = 1000)
summary(m)

saveRDS(m, 'clarke_model.rds')

# plotting
source("../../functions/plot_model.R")

plot_model_fixed(m, dc)
plot_model_spatial(m, dc)
plot_init_sel(m, dc)

