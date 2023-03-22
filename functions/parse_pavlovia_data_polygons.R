parse_exp_data <- function(dr) {
  
  # first, remove first NA rows and gunk
  dr <- dr %>% filter(is.finite(trialNo))
  
  # remove the first trial number 1 as it isn't a real trial
  #dr <- dr[-1,] # currently commented out as no practice trials
  
  # parse x y and class
  dr %>%
    select(trialNo, 144:183) %>% # these are the relevant columns for x and y positions
    pivot_longer(-c(trialNo), names_to = "label", values_to = "z") %>%
    mutate(id = parse_number(label)) %>%
    mutate(z = str_replace_all(z, "\\[|\\]", ""),
           z = str_squish(z)) %>%
    separate(z, into = c("x", "y"), sep = " ") %>%
    mutate(intermediate = row_number(),
           trialNoReal = ceiling(intermediate/40),
           x = as.numeric(x),
           y = as.numeric(y)) %>%
    select(trialNo, trialNoReal, id, x, y) -> d_stim


 # add in number of vertices
  vertices_8 <- c(1:10)
  vertices_7 <- c(11:20)
  vertices_6 <- c(21:40)
  
  d_stim %>% mutate(
    vertices = case_when(
      id %in% vertices_8 ~ 8,
      id %in% vertices_7 ~ 7,
      id %in% vertices_6 ~ 6
    )) %>%
    mutate(person = dr$participant[[1]]) -> d_stim
  
  # if key_resp.rt doesn't exist, create it with a load of NAs
  if (!("key_resp.rt" %in% names(dr))) {
    dr %>% mutate(key_resp.rt = NaN) -> dr
  }
  
  # look at the order in which things were clicked
  dr %>% select(trialNo, mouse.clicked_name, key_resp.rt) %>%
    rename(found  = "mouse.clicked_name", end_trial = "key_resp.rt") %>%
    separate(found, into = as.character(1:40), sep = "," ) %>%
    pivot_longer(-c(trialNo, end_trial), names_to = "found", values_to = "id") %>%
    mutate(id = str_replace_all(id, "\\[|\\]", ""),
           id = str_replace_all(id, "\\'", ""),
           id = str_squish(id),
           id = if_else(id == "polygon", "polygon_1", id),
           id = parse_number(id),
           found = as.numeric(found)) %>%
    filter(is.finite(id)) %>%
    left_join(d_stim, by = c("trialNo", "id")) -> d_found
  
  # now sort out time info
  dr %>% select(trialNo, mouse.time) %>%
    rename(time  = "mouse.time") %>%
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
        mutate(person = dr$participant[[1]]) %>%
        group_by(trialNo) %>%
        mutate(found = row_number()) %>% # making row numbers right again
        ungroup()
    }
  }
  
  rm(dm)
  
  # removing unhelpful trial numbers
  d_stim <- d_stim %>%
    select(-trialNo) %>%
    rename(trialNo = trialNoReal) 
    
  
  d_found <- d_found %>%
    select(-trialNo) %>%
    rename(trialNo = trialNoReal)
  
  # add in exp name (useful for working out conditions later)
  
  d_stim <- d_stim %>%
    mutate(expName = dr$expName[1])
  
  d_found <- d_found %>%
    mutate(expName = dr$expName[1])
  
  return(list(stim = d_stim, found = d_found))
  
  
}
