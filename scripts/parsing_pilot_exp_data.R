library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)

source("../functions/parse_pavlovia_data.R")
source("../functions/prep_data.R")


options(mc.cores = parallel::detectCores())

folder = "../data/pilot Oct 2022/"
files <- dir(folder, ".csv")


# first make a dataframe of all found targets and save
d <- tibble()
d_new <- tibble()
for (pp in 1:length(files)) {
  
  d_new <- parse_exp_data(read_csv(paste0(folder, files[pp]), 
                          show_col_types = FALSE))$found
  
  d_new <- mutate(d_new, file_name = str_remove(files[pp], ".csv"))
  
  d <- rbind(d, d_new)
  
}


write_csv(d, "ps300.csv")

d %>% group_by(person, file_name, trialNo, block) %>% 
  summarise(end_trial = unique(end_trial)) -> dend

write_csv(dend, "space_bar_times.csv")


for (pp in 1:length(files)) {
  d <- parse_exp_data(read_csv(paste0("../data/Pavlovia/", files[pp]),
                               show_col_types = FALSE))
  
  
  if (nrow(d$found) > 500){
  d_list <- prep_data_for_stan(d$found, d$stim)
  m <- stan("../models/foraging_model.stan", data = d_list,
             chains = 4, iter = 500)
  
  
  saveRDS(m, paste0("models/", str_remove(files[pp], ".csv"), ".model"))
  }
}


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

        