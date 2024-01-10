library(tidyverse)

# these functions import data and does some initial processing to get them into 
# our stim and found format. 

# import_data will return a list that contains two dataframes
# d$stimulus contains all of the stimulus information
# d$found tells us which items were selected, and the order in which they were selected. 

# (0, 0) should represent the bottom-left corner of the display.
# as assume the stimulus has width of 1unit, with height depending on the aspect ratio


import_data <- function(dataset, small_test=FALSE) {
  
  d <- switch(dataset,
              "clarke2022qjep" = import_clarke2022qjep(small_test),
              "tagu2022cog"    = import_tagu2022cog(small_test),
              "hughes2024pilot" = import_hughes2024pilot(),
              "unknown dataset")
}


fix_person_and_trial <- function(d) {
  
  # first arrange data so it has all of person 1, then person 2, etc
  d %>% arrange(person, condition, trial) -> d
  
  # make sure the person index goes from 1 to N with no missing people
  d$person <- as_factor(d$person)
  levels(d$person) <- 1:length(levels(d$person))
  d$person <- as.numeric(d$person)
  
  # make sure trial number is unique over people and conditions
  # save the old trial info in trial_p
  d %>% mutate(
    trial_p = trial,
    trial = paste(as.numeric(person), as.numeric(condition), trial),
               trial = as.numeric(as_factor(trial))) -> d
  
  return(d)
  
}

import_hughes2024pilot <- function() {

  d_found <- read_csv(paste0("../output/", exptname, "/d_found.csv")) %>% 
    unite(condition, difficulty, common) %>% 
    mutate(condition = as_factor(condition),
           class = as.numeric(as_factor(class))) %>%
    rename(item_class = "class")
  
  d_stim <- read_csv(paste0("../output/", exptname, "/d_stim.csv")) %>% 
    unite(condition, difficulty, common) %>% 
    mutate(condition = as_factor(condition),
           class = as.numeric(as_factor(class))) %>%
    rename(item_class = "class")
  
  d_found <- fix_person_and_trial(d_found)
  d_stim <- fix_person_and_trial(d_stim)
  
  return(list(stim = d_stim,
              found = d_found))

}

import_tagu2022cog <- function(small_test) {
  
  my_spec <- cols(
    condition = col_double(),
    trial = col_double(),
    observer = col_double(),
    targ_type = col_double(),
    id = col_double(),
    x = col_double(),
    y = col_double(),
    found = col_double())
  
  d <- read_csv("../../data/tagu2022/tagu_2020_prox_mouse.csv", col_types = my_spec) 
  
  # remove trials with NAs 
  na_trls <- filter(d, is.na(targ_type)) %>%
    mutate(key = paste(condition, trial, observer))
  
  d %>% mutate(key = paste(condition, trial, observer)) %>%
    filter(!(key %in% na_trls$key)) %>%
    select(-key) %>% # we don't need this?
    select(person = "observer", condition, trial = "trial",  
           id = "id", found = "found", item_class = "targ_type",
           x = "x", y = "y") %>%
    mutate(item_class = item_class + 1,
           condition = as.factor(condition)) -> d
  
  # if (small_test) {
  #   
  #   d <- filter(d, person < 6, trial < 5)
  # }
  
  d <- fix_person_and_trial(d)
  
  # scale x to (0, 1) and y to (0, a) where a is the aspect ratio
   
  # first subtract the min
  d %>% mutate(x = x - min(x),
               y = y - min(y)) -> d
  
  xmax <- max(d$x)
  
  d %>% mutate(x = x/xmax,
               y = y/xmax) -> d
  
  # flip y coordinates so that (0, 0) is the bottom left
  # d$y = 1-d$y
  # d$y = d$y - min(d$y)
  # 
  # extract stimulus data
  d_stim <- d %>% select(person, condition, trial, id, x, y, item_class, trial_p) %>%
    arrange(person, condition, trial) 
  
  # extract behavioral data
  d_found <- d %>% filter(found > 0) %>% 
    arrange(person, condition, trial, found) 
  
  if (small_test) {
    d_found <- filter(d_found, found == 1)
  }
  
  return(list(stim = d_stim,
              found = d_found))

}


import_clarke2022qjep <- function(small_test) {
  
  my_spec <- cols(
    condition = col_double(),
    trial = col_double(),
    observer = col_double(),
    targ_type = col_double(),
    id = col_double(),
    x = col_double(),
    y = col_double(),
    found = col_double(),
    RT = col_double())
  
  d <- read_csv("../../data/clarke2022/clarke2022qjep_collected.csv", 
                col_types = my_spec)
  
  d  %>%
    select(person = "observer", condition, trial = "trial",  
           id = "id", found = "found", item_class = "targ_type",
           x = "x", y = "y") %>%
    mutate(item_class = item_class + 1,
           condition = as.factor(condition),
           condition = fct_recode(condition, feature = "1", conjunction = "2")) -> d
  
  if (small_test) {

    d <- filter(d, person < 6, trial < 10)
  }
  
  d <- fix_person_and_trial(d)
  
  # flip y coordinates so that (0, 0) is the bottom left
  # d$y = 1-d$y
  # d$y = d$y - min(d$y)
  
  # extract stimulus data
  d_stim <- d %>% select(person, condition, trial, id, x, y, item_class, trial_p) %>%
    arrange(person, condition, trial) 
  
  # extract behavioral data
  d_found <- d %>% filter(found > 0) %>% 
    arrange(person, condition, trial, found) 
  
  if (small_test) {
    d_found <- filter(d_found, found == 1)
  }

  return(list(stim = d_stim,
              found = d_found))
}