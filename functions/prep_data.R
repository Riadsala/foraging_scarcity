
compute_inter_targ_directions <- function(Y, df, ds) {
  
  directions <- array()
  
  for (ii in 1:length(Y)) {
    
    if (df$found[ii]==1) {
      
      #starting a new trial, so get relevant data
      trl_dat = filter(ds, 
                       person == df$person[ii],
                       trial == df$trial[ii])
      
      # as first target in trial, distance will be zero
      directions <- rbind(directions, rep(0, nrow(trl_dat)))
      
    } else {
      
      x <- trl_dat$x - trl_dat$x[Y[ii-1]]
      y <- trl_dat$y - trl_dat$y[Y[ii-1]]
      
      phi <- atan2(y, x)
      
      directions <- rbind(directions, phi)
    }
  }
  
  directions <-  directions[-1,]
  
  rm(trl_dat)
  
  return(directions)
  
}


compute_inter_targ_distances <- function(Y, df, ds) {
  
  distances <- array()
  
  for (ii in 1:length(Y)) {
  
    if (df$found[ii]==1) {
      
      #starting a new trial, so get relevant data
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
  
  return(distances)
  
}


compute_inter_sel_direction <- function(Y, df, ds) {
  
  # function to compute the angular difference between the 
  # vector from target i-2 to i-1 and the vector from 
  # target i-1 to i. 
  
  theta <- array()
  
  for (ii in 1:length(Y)) {
    
    #starting a new trial, so get relevant data
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
  
  return(theta)
  
}

does_item_match_prev_target <- function(Y, df, targ_class, n_targ_per_class, n_targ_class) {
  
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
  
  return(matching)
  
}


prep_data_for_stan <- function(df, ds) {
  
  # make sure trial ids are unique
  df %>% mutate(block = as.factor(block),
                trial = paste(as.numeric(person), as.numeric(block), trial),
                trial = as.numeric(as_factor(trial))) -> df
  
  ds %>% mutate(block = as.factor(block),
                class = as.factor(class),
                trial = paste(as.numeric(person), as.numeric(block), trial),
                trial = as.numeric(as_factor(trial))) -> ds
  
  # correct (x, y) so that neither ever = 0 or 1
  # as this causes problems for Beta distributions
  df %>% mutate(x = as.vector(scales::rescale(x, to = c(0.01, 0.99))),
                y = as.vector(scales::rescale(y, to = c(0.01, 0.99)))) -> df
  
  ds %>% mutate(x = as.vector(scales::rescale(x, to = c(0.01, 0.99))),
                y = as.vector(scales::rescale(y, to = c(0.01, 0.99)))) -> ds
  
  
  # extract stimulus parameters
  n_people <- length(unique(df$person))
  n_trials <- length(unique(df$trial))
  n_targ_class <- length(unique(ds$class))
  n_targ_per_class <- length(unique(ds$id))/n_targ_class
 
  Y = as.numeric(df$id)
  
  # pre-compute distance data
  distances <- compute_inter_targ_distances(Y, df, ds)
  # pre-compute direction data
  phi <- compute_inter_targ_directions(Y, df, ds)
  
  
  # pre-compute relative direction data
  theta <- compute_inter_sel_direction(Y, df, ds) 
  
  
  d_trl <- ds %>% group_by(person, trial) %>% 
    summarise(condition = unique(block), .groups = "drop")
  # remove trials in which no targets were found
  d_trl <- filter(d_trl, trial %in% (df %>% group_by(trial) %>% 
                                       summarise(n=n(), .groups = "drop"))$trial)
  
  X <- as.numeric(d_trl$condition)
  
  targ_class = t(array(as.numeric(ds$class), dim = c(n_targ_per_class*n_targ_class, n_trials)))
  
  # work out which targets match previous target
  matching <- does_item_match_prev_target(Y, df, targ_class, n_targ_per_class, n_targ_class)
  
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
    A = phi,
    E = theta,
    Z = df$person,
    alpha = 2,
    prior_sd_bS = 1,
    prior_mu_phidis = 15,
    prior_sd_phidis = 4,
    prior_mu_phidir = 0,
    prior_sd_phidir = 3,
    prior_mu_floor = -2,
    prior_sd_floor = 2)  
  
  return(d_list)
  
}
