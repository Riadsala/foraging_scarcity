library(tidyverse)

source("../functions/parse_pavlovia_data_polygons.R")
source("../functions/prep_data.R")

options(mc.cores = parallel::detectCores())

# exptname <- "polygon pilot white shapes"
exptname <- "polygon pilot March 2023"

folder = paste0("../data/", exptname, "/")
savefolder = paste0("../output/", exptname, "/")

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

########################################################
# Some data checking #
########################################################

# How many trials does each person have?

d_found %>%
  group_by(person, common, difficulty) %>%
  summarise(total_trials = length(unique(trial))) -> total_trials

# How many targets of each class did people find?

d_found %>%
  count(person, common, difficulty, class) -> total_found

# Do hard trials seem easier?

ggplot(total_found, aes(difficulty, n, fill = class)) + geom_boxplot() + facet_grid(~common)

# Number of runs?

d_found %>%
  group_by(person, common, difficulty, trial) %>%
  mutate(switch = ifelse(class != lag(class), 1, 0)) %>%
  summarise(num_switches = sum(switch, na.rm = TRUE)) -> total_switches

ggplot(total_switches, aes(difficulty, num_switches)) + geom_boxplot()                                  

        