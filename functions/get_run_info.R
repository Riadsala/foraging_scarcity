get_run_info <- function(pp, trl, blk, d) {
  
  # calculate some simple run statistics for a trial
  
  trl_dat <- filter(d, person == pp, trial == trl,  condition == blk) %>% 
    arrange(found)
  
  rl <- rle(trl_dat$class)
  
  return(list(
    person = pp,
    trial = trl,
    block = blk,
    n_found = nrow(trl_dat),
    max_run_length = max(rl$lengths), 
    # all_run_lengths =  list(rl$lengths), 
    n_runs = length(rl$lengths)))
}       
