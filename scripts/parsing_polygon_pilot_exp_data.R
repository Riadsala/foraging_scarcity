library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())

source("../functions/parse_pavlovia_data_polygons.R")
source("../functions/prep_data.R")


options(mc.cores = parallel::detectCores())

folder = "../data/polygon pilot Feb 2023/"
files <- dir(folder, ".csv")


# first make a dataframe of all found targets and save
d_found <- tibble()
d_stim <- tibble()

for (pp in 1:length(files)) { 

d_new <- parse_exp_data(read_csv(paste0(folder, files[pp]), 
                                   show_col_types = FALSE))
d_found_pp <- d_new$found
d_stim_pp <- d_new$stim

d_found <- rbind(d_found, d_found_pp)
d_stim <- rbind(d_stim, d_stim_pp)
  
rm(d_new, d_found_pp, d_stim_pp)

}

# remove distractor clicks (as these will no appear in final pilot data)

trials_with_6 <- d_found %>% group_by(person, expName, trialNo) %>%
  summarise(click6 = sum(vertices==6)) %>%
  filter(click6>0) %>%
  unite(cond, person, expName, trialNo)

d_found %>%
  unite(cond, person, expName, trialNo, remove=F) %>%
  filter(!(cond %in% trials_with_6$cond)) %>%
  select(-cond) -> d_found

  d_stim %>%
    unite(cond, person, expName, trialNo, remove=F) %>%
  filter(!(cond %in% trials_with_6$cond)) %>%
    select(-cond) -> d_stim

# 

d_found %>% rename(trial = trialNo, class = vertices) %>%
  mutate(expName = str_remove(expName, "foraging_expt_polygons_"),
         block = as_factor(expName),
         class = as.numeric(as_factor(class)))  -> d_found


d_stim %>% rename(trial = trialNo, class = vertices) %>%
  filter(class != 6) %>%
           mutate(expName = str_remove(expName, "foraging_expt_polygons_"),
                  block = as_factor(expName),
         class = as.numeric(as_factor(class)))-> d_stim
  

######################################################
# now prepare data for stan model
######################################################


# d_found %>% filter(person == 1, block == 1, trial == 1) %>%
  # ggplot(aes(x, y)) + 
  # geom_label(aes(label = found, colour = as.factor(class))) + 
  # geom_path(data = d_found %>% filter(person == 1, block == 1, trial == 1, found>0),size = 1)
  # 



d_list <- prep_data_for_stan(d_found, d_stim)

d_list$targ_class <- d_list$targ_class-1

m <- stan("../../foraging_spatial/models/foraging_model1_nonml.stan", data = d_list, 
          chains = 1, iter = 1000)


blks_labels <- levels(d_found$block)

source("../functions/plot_model_rr.R")


plot_model_fixed(m, d_found, blks_labels)
        