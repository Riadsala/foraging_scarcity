parse_exp_data <- function(dr) {
  
  # first, remove first NA rows and gunk
  dr <- dr %>% filter(is.finite(trialNo))
  
  # remove the first trial number 1 as it isn't a real trial
  dr <- dr[-1,]
  
  # # get number of blocks
  # n_blocks <- length(which(is.na(dr$trialNo)))
  # blk_labels <- filter(dr, is.na(trialNo))$block_name
  # 
  dr <- dr %>% filter(is.finite(trialNo))
  
  dr <- dr %>% 
    # mutate(block = rep(blk_labels, each = nrow(dr)/n_blocks)) %>%
    rename(trial = "trialNo") %>%
    mutate(trial = 1:n()) -> dr
  
  # parse x y and class
  dr %>%
    select(trial, contains(c("_x", "_y"))) %>%
    pivot_longer(-c(trial), names_to = "label", values_to = "z") %>%
    separate(label, into = c("id", "d")) %>%
    pivot_wider(c(trial, id), names_from = "d", values_from = "z") %>%
    mutate(id = parse_number(id)) -> d_stim
  
  # add in sat and colour info
  item_scarce <- c(1:5)
  item_even <- c(6:10)
  item_common <- 
  
  d_stim %>% mutate(
    saturation = if_else(id %in% sat, "saturated", "unsaturated"),
    colour = if_else(id %in% red, "red", "green"),
    class = paste(saturation, colour)) %>%
    select(-colour, -saturation) %>%
    mutate(person = pp) -> d_stim
  
  # if key_resp.rt doesn't exist, create it with a load of NAs
  if (!("key_resp.rt" %in% names(dr))) {
    dr %>% mutate(key_resp.rt = NaN) -> dr
  }
  
  # look at the order in which things were clicked
  dr %>% select(trial, mouse_temp.clicked_name, key_resp.rt) %>%
    rename(found  = "mouse_temp.clicked_name", end_trial = "key_resp.rt") %>%
    separate(found, into = as.character(1:40), sep = "," ) %>%
    pivot_longer(-c(trial, end_trial), names_to = "found", values_to = "id") %>%
    mutate(id = if_else(id == "\"polygon\"", "\"polygon_1\"", id),
           id = if_else(id == "[\"polygon\"", "\"polygon_1\"", id),
           id = if_else(id == "\"polygon\"]", "\"polygon_1\"", id),
           id = parse_number(id),
           found = as.numeric(found)) %>%
    filter(is.finite(id)) %>%
    left_join(d_stim, by = c("trial", "id")) -> d_found
  
  # now sort out time info
  dr %>% select(trial, mouse_temp.time) %>%
    rename(time  = "mouse_temp.time") %>%
    separate(time, into = as.character(1:40), sep = "," ) %>%
    pivot_longer(-c(trial), names_to = "found", values_to = "time") %>%
    mutate(time = round(parse_number(time), 3),
           found = as.numeric(found)) %>%
    right_join(d_found) -> d_found
  
  
  
  # check for double clicks on targets
  
  d_found %>% group_by(trial, id) %>%
    summarise(n = n()) %>%
    filter(n > 1) -> dm
  if (nrow(dm) > 0) {
    for (ii in 1:nrow(dm)) {
      
      d_found %>% filter(trial == dm$trial[ii], id == dm$id[ii]) %>%
        filter(found != min(found)) -> to_remove
      
      d_found <- filter(d_found, !(trial %in% to_remove$trial & 
                                     id %in% to_remove$id &
                                     found %in% to_remove$found)) %>%
        mutate(person = pp) %>%
        group_by(trial) %>%
        mutate(found = row_number()) %>% # making row numbers right again
        ungroup()
    }
  }
  
  rm(dm)
  
  return(list(stim = d_stim, found = d_found))
  
  
}
