prep_data_for_stan <- function(df, ds) {
  
  # make sure trial ids are unique
  
  df %>% mutate(block = as.factor(block),
                trial = paste(as.numeric(block), trial),
                trial = as.numeric(as_factor(trial))) -> df
  
  ds %>% mutate(block = as.factor(block),
                class = as.factor(class),
                trial = paste(as.numeric(block), trial),
                trial = as.numeric(as_factor(trial))) -> ds
  
  # extract stimulus parameters
  n_people <- length(unique(df$person))
  n_trials <- length(unique(df$trial))
  n_targ_class <- length(unique(ds$class))
  n_targ_per_class <- length(unique(ds$id))/n_targ_class
 
  Y = as.numeric(df$id)
  
  # pre-compute distance data
  distances <- array()
  
  for (ii in 1:length(Y)) {
    if (df$found[ii]==1) {
      
      trl_dat = filter(ds, 
                       person == df$person[ii],
                       trial == df$trial[ii])
      
      # as first target in trial, distance will be zero
      distances <- rbind(distances, rep(0, nrow(trl_dat)))
      
    } else {
      x <- trl_dat$x - trl_dat$x[Y[ii-1]]
      y <- trl_dat$y - trl_dat$y[Y[ii-1]]
      
      distances <- rbind(distances, sqrt(x^2 + y^2))
    }
  }
  
  distances <-  distances[-1,]
  
  rm(trl_dat)
  
  # pre-compute direction data
  theta <- array()
  
  for (ii in 1:length(Y)) {
    
    trl_dat = filter(ds, 
                     person == df$person[ii],
                     trial == df$trial[ii])
    
    if (df$found[ii] %in% c(1, 2)) {
      
      # as first or second target in trial, direction will be zero
      phi <- rep(0, nrow(trl_dat))
      
    } else {
      
      d_targ <- df[ii-1,]
      
      if ((d_targ$class == 0)) {
        phi <- rep(0, nrow(trl_dat))
        
      } else {
        d_prev_targ <- filter(df, 
                              person == df$person[ii],
                              trial == df$trial[ii],
                              found == df$found[ii] - 2)
        
        phi = atan2((d_targ$y -  d_prev_targ$y), (d_targ$x -  d_prev_targ$x)) * 180/pi
        phi = (atan2((trl_dat$y - d_targ$y), (trl_dat$x - d_targ$x)) * 180/pi) - phi 
        phi = pmin(abs((phi %% 360)), abs((-phi %% 360)))
        phi = phi/180
        #phi[ii] = 1
      }
    }
    
    theta <- rbind(theta, phi[trl_dat$id])
    
  }
  
  theta <-  theta[-1,]
  
  d_trl <- ds %>% group_by(person, trial) %>% summarise(condition = unique(block))
  # remove trials in which no tragets were found
  d_trl <- filter(d_trl, trial %in% (df %>% group_by(trial) %>% summarise(n=n()))$trial)
  
  X <- as.numeric(d_trl$condition)
  
  targ_class = t(array(as.numeric(ds$class), dim = c(n_targ_per_class*n_targ_class, n_people*n_trials)))
  
  # work out which targets match previous target
  matching = array()
  trl <- 1
  for (ii in 2:length(Y)) {
    if (df$found[ii]==1) {trl = trl + 1}
    
    if (df$class[ii-1]== 0) {
      found_class <- rep(0, n_targ_per_class*n_targ_class)
      
    } else {
      found_class <- targ_class[trl, Y[ii-1]]
      
    }
    
    matching <- rbind(matching, as.numeric(targ_class[trl,] == found_class))
    
  }
  
  matching[which(matching == 0 )] = -1
  matching[which(is.na(matching))] = 0
  
  # get x y coords of all items
  ds %>% select(person, trial, id, x) %>%  
    pivot_wider(names_from = "id", values_from = "x") %>%
    select(-person, -trial) -> itemX
  
  ds %>% select(person, trial, id, y) %>%  
    pivot_wider(names_from = "id", values_from = "y") %>%
    select(-person, -trial) -> itemY
  
  d_list <- list(
    N = nrow(df),
    L = n_people,
    K = length(unique(ds$block)),
    n_trials = n_trials,
    n_targets = n_targ_class*n_targ_per_class,
    n_classes = n_targ_class,
    Y = Y,
    trial_start = df$found,
    targ_class = targ_class,
    X = array(X),
    itemX = itemX, 
    itemY = itemY, 
    S = matching,
    D = distances,
    E = theta,
    Z = df$person,
    alpha = 2,
    prior_mu_bS = 1.1,
    prior_sd_bS = 2,
    prior_mu_phidis = 15,
    prior_sd_phidis = 5,
    prior_mu_phidir = 0,
    prior_sd_phidir = 2,
    prior_mu_floor = -3,
    prior_sd_floor = 1.5)  
  
  return(d_list)
  
}