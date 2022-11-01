parse_exp_data <- function(dr) {
  
  # first, remove first NA rows and gunk
  dr <- dr %>% filter(is.finite(trialNo))
  
  # remove the first trial number 1 as it isn't a real trial
  dr <- dr[-1,]
  
  # parse x y and class
  dr %>%
    select(trialNo, contains(c("_pos"))) %>%
    pivot_longer(-c(trialNo), names_to = "label", values_to = "z") %>%
    separate(label, into = c("id", "d")) %>%
    mutate(z = str_remove(z, "\\["),
           z = str_remove(z, "\\]"),
           z = str_squish(z)) %>%
    mutate(z = str_replace(z, "\\s", "|")) %>%
    separate(z, into = c("x", "y"), sep = "\\|") %>%
    mutate(id = parse_number(id),
           x = parse_number(x),
           y = parse_number(y)) %>%
    select(-d) -> d_stim
  
  # add in colour info
  dr %>%
    select(trialNo, contains(c("colour"))) %>%
    pivot_longer(-c(trialNo), names_to = "label", values_to = "colour") %>%
    mutate(colour = str_replace(colour, "medium", ""),
           id = parse_number(label)) %>%
    select(trialNo, id, colour) -> d_colour
 
  # joining things together 
  d_stim %>% left_join(d_colour) -> d_stim
  
  # add in block info
  d_stim %>%
    mutate(block = case_when(
      trialNo %% 3 == 2 ~ "purple_majority",
      trialNo %% 3 == 0 ~ "purple_minority",
      TRUE ~ "equal"
    )) -> d_stim
  
  # if key_resp.rt doesn't exist, create it with a load of NAs
  if (!("key_resp_temp.rt" %in% names(dr))) {
    dr %>% mutate(key_resp_temp.rt = NaN) -> dr
  }
  
  # look at the order in which things were clicked
  dr %>% select(trialNo, mouse_temp.clicked_name, key_resp_temp.rt) %>%
    rename(found  = "mouse_temp.clicked_name", end_trial = "key_resp_temp.rt") %>%
    separate(found, into = as.character(1:40), sep = "," ) %>%
    pivot_longer(-c(trialNo, end_trial), names_to = "found", values_to = "id") %>%
    mutate(id = parse_number(id),
           found = as.numeric(found)) %>%
    filter(is.finite(id)) %>%
    left_join(d_stim, by = c("trialNo", "id")) -> d_found
  
  # now sort out time info
  dr %>% select(trialNo, mouse_temp.time) %>%
    rename(time  = "mouse_temp.time") %>%
    separate(time, into = as.character(1:40), sep = "," ) %>%
    pivot_longer(-c(trialNo), names_to = "found", values_to = "time") %>%
    mutate(time = round(parse_number(time), 3),
           found = as.numeric(found)) %>%
    right_join(d_found) -> d_found
  
  
  
  # check for double clicks on targets
  d_found %>% group_by(trialNo, id) %>%
    summarise(n = n()) %>%
    filter(n > 1) -> dm
  if (nrow(dm) > 0) {
    for (ii in 1:nrow(dm)) {
      
      d_found %>% filter(trialNo == dm$trialNo[ii], id == dm$id[ii]) %>%
        filter(found != min(found)) -> to_remove
      
      d_found <- filter(d_found, !(trialNo %in% to_remove$trialNo & 
                                     id %in% to_remove$id &
                                     found %in% to_remove$found)) %>%
        mutate(person = pp) %>%
        group_by(trialNo) %>%
        mutate(found = row_number()) %>% # making row numbers right again
        ungroup()
    }
  }
  
  rm(dm)
  
  return(list(stim = d_stim, found = d_found))
  
  
}
