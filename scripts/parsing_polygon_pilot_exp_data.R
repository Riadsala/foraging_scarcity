library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())

source("../functions/parse_pavlovia_data_polygons.R")
source("../functions/prep_data.R")


options(mc.cores = parallel::detectCores())

folder = "../data/polygon pilot Nov 2022/"
files <- dir(folder, ".csv")


# first make a dataframe of all found targets and save

d_new <- parse_exp_data(read_csv(paste0(folder, files[pp]), 
                                   show_col_types = FALSE))
d_found <- d_new$found
d_stim <- d_new$stim
  
rm(d_new)

# remove distractor clicks (as these will no appear in final pilot data)

d_found <- filter(d_found, vertices != 6)
d_stim <- filter(d_stim, vertices != 6)

# 
# d %>% group_by(person, trialNo, vertices) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   group_by(person, vertices) %>%
#   summarise(mean_found = mean(n)) -> df

d_found %>% rename(trial = trialNo, class = vertices) %>%
  mutate(block = 1,
         class = as.numeric(as_factor(class))) -> d_found


d_stim %>% rename(trial = trialNo, class = vertices) %>%
  mutate(block = 1,
         class = as.numeric(as_factor(class))) -> d_stim
  

######################################################
# now prepare data for stan model
######################################################


d_found %>% filter(person == 1, block == 1, trial == 1) %>%
  ggplot(aes(x, y)) + 
  geom_label(aes(label = found, colour = as.factor(class))) + 
  geom_path(data = d_found %>% filter(person == 1, block == 1, trial == 1, found>0),size = 1)

d_list <- prep_data_for_stan(d_found, d_stim)



m <- stan("../models/foraging_model_no_init_sel.stan", data = d_list, 
          chains = 1, iter = 1000)




w <- spread_draws(m, 
                  post_weights_sel[N],
                  post_weights_sum[N],
                  ndraws = 100) %>%
  median_hdci() %>%
  select(N, post_weights_sel, post_weights_sum) %>%
  mutate(found = d_list$trial_start,
         condition = d$found$block,
         time = d$found$time,
         trial = d$found$trial)


w %>% mutate(time_diff = time - lag(time),
             time_diff = if_else(found == 1, time, time_diff),
             sel_prop = post_weights_sel / post_weights_sum)  -> w
   
  
  


w %>% filter(found > 1) %>%
  ggplot(aes(found, log(post_weights_sel),
             colour = condition,
             group = trial)) + geom_jitter(alpha = 0.50) + 
  geom_path(alpha = 0.5)

w %>% filter(found > 1) %>%
  ggplot(aes(post_weights_sel, log(time_diff),
             colour = condition)) + geom_jitter(alpha = 0.50) +
  geom_smooth(method  = "lm")



summary(lm(log(time_diff) ~  condition * sel_prop,
           filter(w, found > 0, time_diff != 0)))

        