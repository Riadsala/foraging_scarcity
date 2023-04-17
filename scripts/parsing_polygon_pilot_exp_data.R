library(tidyverse)
library(rstan)
library(tidybayes)
library(patchwork)
options(mc.cores = parallel::detectCores())

source("../functions/parse_pavlovia_data_polygons.R")
source("../functions/prep_data.R")


options(mc.cores = parallel::detectCores())

folder = "../data/polygon pilot feature conjunction/"
savefolder = "../output/"

files <- dir(folder, ".csv")

# set ggplot2 theme
theme_set(theme_bw())

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

write_csv(d_found, paste0(savefolder, "d_found.csv"))
write_csv(d_stim, paste0(savefolder, "d_stim.csv"))

######################################################
# now prepare data for stan model
######################################################


# d_found %>% filter(person == 1, block == 1, trial == 1) %>%
  # ggplot(aes(x, y)) + 
  # geom_label(aes(label = found, colour = as.factor(class))) + 
  # geom_path(data = d_found %>% filter(person == 1, block == 1, trial == 1, found>0),size = 1)
  # 


        