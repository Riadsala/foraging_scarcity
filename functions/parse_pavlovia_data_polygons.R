parse_found_sting <- function(participant, found, trial) {
  
  found <- str_extract_all(found, "polygon(_[1234]*\\d)*")
  found <- unlist(found)
  found <- str_replace(found, "polygon$", "polygon_1")
  found <- str_remove(found, "polygon_")
  found <- parse_number(found)
  
  # remove 2nd (and 3rd etc) clicks on targets
  to_remove = as.numeric()
  
  if (length(found) > 1) {
    for (ii in 2:length(found))  {
      if (found[ii] %in% found[1:(ii-1)]) 
        to_remove <- append(to_remove, ii)
    }
  }
  
  if (length(to_remove)>0)  found = found[-to_remove]
  
  return(tibble(
    participant = participant, 
    trial = trial, 
    id = found, 
    found = 1:length(found)))
}

parse_exp_data <- function(dr) {
  
  # first, remove first NA rows and gunk
  dr <- dr %>% filter(is.finite(trialNo))
  
  # remove the first trial number 1 as it isn't a real trial
  #dr <- dr[-1,] # currently commented out as no practice trials
  
  # parse x y and class
  dr %>%
    select(expName, participant, trialNo, p1:p20) %>% # these are the relevant columns for x and y positions
    pivot_longer(-c(expName, participant, trialNo), names_to = "label", values_to = "z") %>%
    mutate(id = parse_number(label)) %>%
    mutate(z = str_replace_all(z, "\\[|\\]", ""),
           z = str_squish(z)) %>%
    separate(z, into = c("x", "y"), sep = " ") %>%
    mutate(
      participant = parse_integer(participant),
      intermediate = row_number(),
           trial = ceiling(intermediate/20), # create a new trial number every 20 targets
           x = as.numeric(x),
           y = as.numeric(y)) %>%
    select(condition = "expName", participant, trial, id, x, y) %>%
    filter(is.finite(trial)) %>%
    separate(condition, c("foraging","expt", "polygons", "difficulty", "common")) %>%
    select(-foraging, -expt, -polygons)  -> d_stim
  
  # check that we have sensible d_stim
  # d_stim %>% group_by(trial) %>%
  #   summarise(n = n())

 # add in number of vertices
  d_stim %>%
    mutate(
      class = case_when(
        (common == "A" & id < 16) ~ "A",
        (common == "A" & id > 15) ~ "B",
        (common == "B" & id < 6) ~ "A",
        (common == "B" & id > 5) ~ "B",
        (common == "AB" & id < 11) ~ "A",
        (common == "AB" & id > 10) ~ "B")) -> d_stim

 
  # # if key_resp.rt doesn't exist, create it with a load of NAs
  # if (!("key_resp.rt" %in% names(dr))) {
  #   dr %>% mutate(key_resp.rt = NaN) -> dr
  # }
  
  # look at the order in which things were clicked
  dr %>% select(participant, mouse.clicked_name) %>%
    rename(found  = "mouse.clicked_name") %>%
    mutate(trial = 1:n(),
           participant = parse_number(participant)) -> d_found
  
  d_found <- pmap_df(d_found, parse_found_sting)
  
  # remove any trial with a distracter click
  d_found %>%
    group_by(trial) %>%
    summarise(yes = sum(id > 20)) %>%
    filter(yes > 0) -> to_remove
  
  d_found %>% filter(!(trial %in% to_remove$trial)) %>%
    left_join(d_stim, by = c("participant", "trial", "id")) -> d_found
  
  # remove trials from d_stim that were scrapped (from d_found)
  trials_to_keep <- unique(d_found$trial)
  
  d_stim %>% filter(trial %in% trials_to_keep) -> d_stim
  
  # now sort out time info
  # dr %>% select(trialNo, mouse.time) %>%
  #   rename(time  = "mouse.time") %>%
  #   separate(time, into = as.character(1:40), sep = "," ) %>%
  #   pivot_longer(-c(trialNo), names_to = "found", values_to = "time") %>%
  #   mutate(time = round(parse_number(time), 3),
  #          found = as.numeric(found)) %>%
  #   right_join(d_found) -> d_found
  
  
  # check for double clicks on targets
 

  return(list(stim = d_stim, found = d_found))
  
  
}
