#################################################################
#
# List of Functions in this script
# 
# < fill in>
# convert_class_weights_to_pA - converts a ratio to a proportion
#################################################################

sim_foraging_people <- function(n_people = 10,
                    n_conditions = 2,
                    n_trials_per_cond = 10,
                    n_targ_class = 3, n_targ_per_class = 2, 
                    targ_class_weights, phi_class_weights,
                    bS = 0, phi_stick = 1,
                    sig_d = 0, phi_d = 5,
                    sig_theta = 0, phi_theta = 1) {
  
  ## if some params have been specified as a constant, replicate over conditions.
  bS <- check_and_rep_param(bS, n_conditions)
  sig_d <- check_and_rep_param(sig_d, n_conditions)
  sig_theta <- check_and_rep_param(sig_theta, n_conditions)
  n_trials_per_cond <- check_and_rep_param(n_trials_per_cond, n_conditions)

  # generate random effects
  dpeeps <- tibble(person = rep(1:n_people, n_conditions),
                   block  = rep(1:n_conditions, each = n_people),
                   mu_cw = rep(targ_class_weights, each = n_people),
                   sd_cw = phi_class_weights,
                   mu_stick = rep(bS, each = n_people),
                   sd_stick = phi_stick,
                   mu_d = rep(sig_d, each = n_people),
                   sd_d = phi_d,
                   mu_theta = rep(sig_theta, each = n_people),
                   sd_theta = phi_theta) 
    
  dpeeps <- pmap_df(dpeeps, gen_random_fx)

  d <- pmap_dfr(dpeeps, sim_foraging_person, 
                n_trials_per_cond = n_trials_per_cond,
                n_targ_class = n_targ_class, 
                n_targ_per_class = n_targ_per_class)
  return(d)
  
}

gen_random_fx <- function(person, block, 
                          mu_cw, sd_cw,
                          mu_stick, sd_stick,  
                          mu_d, sd_d, 
                          mu_theta, sd_theta) {
  
  pA <- mu_cw[1]/sum(mu_cw)
  bA <- boot::logit(pA) + rnorm(1, 0, sd_cw)
  pA <- boot::inv.logit(bA)
  mu_cw = c(pA/(1-pA), 1)
  
  mu_cw[1] <- mu_cw[1] + rnorm(1, 0, sd_cw)
  mu_cw[1] <- if_else(mu_cw[1]<0, 0, mu_cw[1])
  
  dout <- tibble(person, block,
                 targ_class_weights = list(mu_cw),
                 bS = rnorm(1, mu_stick, sd_stick),
                 sig_d = rnorm(1, mu_d, sd_d),
                 sig_theta = rnorm(1, mu_theta, sd_theta))
  
  return(dout)
  
  
}


sim_foraging_person <- function(person = 1,
                                bS, sig_d, sig_theta,
                                block = 1,
                                n_trials_per_cond = 10,
                                n_targ_class = 2, n_targ_per_class, 
                                targ_class_weights) {
  
  trls <- 1:n_trials_per_cond[block]
  
  d <- map_df(trls, sim_foraging_trial, 
              n_targ_class =  n_targ_class, n_targ_per_class = n_targ_per_class[[block]],
              targ_class_weights = targ_class_weights,
              bS = bS, sig_d = sig_d, sig_theta = sig_theta) %>%
    mutate(condition = block)
  
  d %>% mutate(person = person) %>% 
    relocate(person, condition) -> d
  
  return(d)
}


sim_foraging_trial <- function(trl = 1, 
                               n_targ_class = 2, n_targ_per_class = c(5, 15), 
                               targ_class_weights = c(0.5, 0.5),
                               bS = 0, 
                               sig_d = 0, sig_theta = 0)  
{
  
  # n_class is the number of different target classes
  # n_per_type is the number of target's per class
  n_targ_per_class <- check_and_rep_param(n_targ_per_class, n_targ_class)
  
  # targ_class_weights is the salience score for target type
  targ_class_weights <- check_and_rep_param(targ_class_weights, n_targ_class)

  targ_class_weights <- targ_class_weights / sum(targ_class_weights)
  
  # bS is the stick v switch preference
  # sig_d and sig_theta define the spatial bias
  # trl is the trial number
  
  # calculate total number of targets
  n <- sum(n_targ_per_class)
  
  # set up dataframe for storing things
  d_trial <- tibble( x = rep(c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), each = 5),
                     y = rep(c(0.4, 0.2, 0, -0.2, -0.4), times = 8)) %>%
    sample_frac(0.5) %>%
    mutate(
      trial = trl,
    id = 1:n,
    class = rep(1:n_targ_class, n_targ_per_class),
    found = -1)  %>%
    mutate(x = x + runif(n(), -0.02, 0.02),
           y = y + runif(n(), -0.05, 0.05),
           x = x - min(x),
           y = y - min(y),
           x = x/max(x),
           y = y/max(y))

  # d_trial <-
  #   tibble(
  #     trial = trl,
  #     id = 1:n,
  #     x  = runif(n),
  #     y = runif(n),
  #     class = rep(1:n_targ_class, n_targ_per_class),
  #     found = -1)
  # 
  
  # pick a first point at random, 
  # b is based only on the targ_class_weights
  
  q_id <- max(d_trial$id) + 1
  
  d_remain <- d_trial %>%
    mutate(
      dist = 0,
      phi = 0,
      prox = 0,
      b = targ_class_weights[class]) %>%
    mutate(bs = b/sum(b))

  
  # pick a first point at random
  t <- 1
  
  d_found <- sample_n(d_remain, 1, weight = bs) %>%
    mutate(found = 1)
  
  d_trial$found[d_found$id[1]] <- 1
  
  if (d_found$class != 0) {
  
    # remove this point from the stimuli
    d_remain <- filter(d_remain, id != d_found$id, class>0)
    
    ## Now decide if we are stopping already:
    keep_searching <- TRUE
  } else {
    keep_searching <- FALSE
  }
  
  while(keep_searching) {
    
    t <- t + 1

    # compare to previous target
    prev_targ <- d_found$class[t-1]
    match_prev = if_else(d_remain$class == prev_targ, 1, -1)
    
    d_remain %>% mutate(
      dist = (d_found$x[t-1] - x)^2 + (d_found$y[t-1] - y)^2,
      dist = sqrt(dist)) -> d_remain
    
    if (t > 2)  {
      # compute proximity of remaining targets from current target
      d_remain %>% mutate(
        phi = atan2(( d_found$y[t-1] - d_found$y[t-2]), (d_found$x[t-1] - d_found$x[t-2])) * 180/pi,
        phi = (atan2((y - d_found$y[t-1]), (x - d_found$x[t-1])) * 180/pi) - phi ,
        phi = pmin(abs((phi %% 360)), abs((-phi %% 360))),
        phi = phi/180,
        prox = exp(-sig_d * dist - sig_theta * phi)) -> d_remain
     
    } else {
      d_remain %>% mutate(prox = exp(-sig_d * dist)) -> d_remain
    }
    
    d_remain %>% 
      mutate(
        b = targ_class_weights[class] * boot::inv.logit(bS * match_prev),
        b = b * prox) %>%
      mutate(bs = b/sum(b)) -> d_remain
    
    # sample the next target
    d_found %>% add_row(
      sample_n(d_remain, 1, weight = bs)) -> d_found
    
    d_trial$found[d_found$id[t]] <- t
    
    if (d_found$class[t] != 0) {
    
      d_remain <- filter(d_remain, id != d_found$id[t], class>0)
     
      
    } else {
      keep_searching <- FALSE
    }
    
    # check whether we will stop searching now
    if (t == n) {
      keep_searching <- FALSE
    }
    
  }
  
  # add in sim params to d_trial
  d_trial %>% mutate(
    bA = boot::logit(targ_class_weights[[1]]),
    bS = bS, 
    sig_d = sig_d, sig_theta = sig_theta) -> d_trial
  
  # order by found
  d_trial %>% arrange(found) -> d_trial
  
  return(d_trial)
}


check_and_rep_param <- function(p, r) {
  
  if (length(p) == 1) {
    p <- rep(p, r)
  }
  
  return(p)
}

convert_class_weights_to_pA <- function(cW) {
  
  pA <- map_dbl(targ_class_weights,  
                function(x) sum(x[[1]]/sum(x)))
  
  return(pA)
  
}