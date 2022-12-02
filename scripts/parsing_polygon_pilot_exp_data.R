library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())

source("../functions/parse_pavlovia_data_polygons.R")
source("../functions/prep_data.R")


options(mc.cores = parallel::detectCores())

folder = "../data/polygon pilot Dec 2022/"
files <- dir(folder, ".csv")


# first make a dataframe of all found targets and save
#d <- tibble()
#d_new <- tibble()

#for (pp in 1:length(files)) {

pp <- 2 # for testing
  
d_new <- parse_exp_data(read_csv(paste0(folder, files[pp]), 
                                   show_col_types = FALSE))
d_found <- d_new$found
d_stim <- d_new$stim
  
#rm(d_new)

#}

# remove distractor clicks (as these will no appear in final pilot data)

d_found <- filter(d_found, vertices != 6)
d_stim <- filter(d_stim, vertices != 6)

# 

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


        