functions{

  vector standarise_weights(vector w, int n_targets, vector remaining_items) {

    /* set weights of found items to 0 and divide by the sum of 
    remaining weights so that they sum to 1 */
    vector[n_targets] w_s = w .* remaining_items;  

    w_s = w_s / sum(w_s);

    return(w_s);
  }
 
  vector compute_spatial_weights(
    int n, int n_targets, int ii,
    real u_delta, real u_psi, vector delta, vector psi, vector phi,
    vector x, vector y) {

    // functions for calculating the spatial weights

    vector[n_targets] w;

    w = rep_vector(1, n_targets); 

    // now start computing the weights
    if (n == 1) {

      // initial item selection.. no spatial weights for now

    } 
    else if (n == 2) {

      // for the second selected target, weight by distance from the first
      w =  exp(-u_delta*delta);
          
    } else {

      // for all later targets, also weight by direction
      w =  exp(-u_delta*delta - u_psi*psi);

    }

    return(w);

  }
}

data {
  int <lower = 1> N; // total number of selected targets over the whole experiment
  int <lower = 1> L; // number of participant levels 
  int <lower = 1> K; // number of experimental conditions  

  int <lower = 1> n_trials;  // total number of trials (overall)
  int <lower = 1> n_classes; // number of target classes - we assume this is constant over n_trials
  int <lower = 1> n_targets; // total number of targets per trial
  array[N] int <lower = 0, upper = n_targets> found_order; // = 1 is starting a new trial, 0 otherwise

  array[N] int <lower = 1> Y; // target IDs - which target was selected here? This is what we predict

  // (x, y) coordinates of each target
  array[n_trials] vector<lower=0,upper=1>[n_targets] item_x;
  array[n_trials] vector<lower=0,upper=1>[n_targets] item_y;

  array[N] vector<lower = 0>[n_targets] delta; // distance measures
  array[N] vector[n_targets] psi; // direction measures (relative)
  array[N] vector[n_targets] phi; // direction measures (absolute)

  array[n_trials] int <lower = 1, upper = K> X; // trial features (ie, which condition are we in)
  matrix<lower = -1, upper = 1>[n_trials, n_targets] item_class; // target class, one row per trial
  array[N] vector<lower = -1, upper = 1>[n_targets] S; // stick/switch (does this targ match prev targ) 
  array[N] int <lower = 1, upper = L> Z; // random effect levels
  array[N] int<lower = 1, upper = n_trials> trial; // what trial are we on? 

  // read in priors
  real prior_mu_bA; // param for class weight prior
  real prior_sd_bA; // param for class weight prior
  real prior_mu_b_stick; // prior for sd for bS
  real prior_sd_b_stick; // prior for sd for bS
  real prior_mu_rho_delta;
  real prior_sd_rho_delta;
  real prior_mu_rho_psi;
  real prior_sd_rho_psi;
}

transformed data{

  array[N] vector[n_targets] remaining_items;

  for (n in 1:N) {
        // check if we are at the start of a new trial
    // if we are, initialise a load of things
    if (found_order[n] == 1) {
             
      // as we're at the start of a new trial, reset the remaining_items tracker
      remaining_items[n] = rep_vector(1, n_targets);

    } else {

      remaining_items[n] = remaining_items[n-1];
      remaining_items[n][Y[n-1]] = 0;
      
    }
  }
}

parameters {
  // These are all the parameters we want to fit to the data

  ////////////////////////////////////
  // fixed effects
  ////////////////////////////////////

  /* in order to allow for correlations between the
  variables, these are all stored in a list
  these include bA, bS (stick weight), and the two spatial 
  sigmas, along with the floor (chance of selectin an 
  item at random)
  */
  array[K] real bA; // weights for class A compared to B  
  array[K] real b_stick; // stick-switch rates 
  array[K] real<lower = 0> rho_delta; // distance tuning
  array[K] real rho_psi; // direction tuning

  ///////////////////////////////
  // random effects
  ///////////////////////////////

  array[K] vector[L] zA; // weights for class A compared to B  
  array[K] vector[L] z_stick; // stick-switch rates 
  array[K] vector[L] z_delta; // distance tuning
  array[K] vector[L] z_psi; // direction tuning

  real<lower = 0> sig_a;
  real<lower = 0> sig_stick;
  real<lower = 0> sig_delta;
  real<lower = 0> sig_psi;
  
}

transformed parameters {

  // combine fixed and random effects
  array[K] vector[L] uA; 
  array[K] vector[L] u_stick; 
  array[K] vector[L] u_delta; 
  array[K] vector[L] u_psi; 

  for (kk in 1:K) {
    uA[kk] = bA[kk] + sig_a*zA[kk];
    u_stick[kk] = b_stick[kk] + sig_stick*z_stick[kk];
    u_delta[kk] = rho_delta[kk] + sig_delta*z_delta[kk];
    u_psi[kk]   = rho_psi[kk] + sig_psi*z_psi[kk];
  }
}

model {

  /////////////////////////////////////////////////////
  // Define Priors
  ////////////////////////////////////////////////////
  for (ii in 1:K) {
    // priors for fixed effects
    target += normal_lpdf(bA[ii]        | 0, prior_sd_bA);
    target += normal_lpdf(b_stick[ii]   | 0, prior_sd_b_stick);
    target += normal_lpdf(rho_delta[ii] | prior_mu_rho_delta, prior_sd_rho_delta);
    target += normal_lpdf(rho_psi[ii]   | prior_mu_rho_psi, prior_sd_rho_psi);

    target += normal_lpdf(zA[ii]      | 0, 1);
    target += normal_lpdf(z_stick[ii] | 0, 1);
    target += normal_lpdf(z_delta[ii] | 0, 1);
    target += normal_lpdf(z_psi[ii]   | 0, 1);

  }

  // priors for group variance
  target += exponential_lpdf(sig_a     | 1);
  target += exponential_lpdf(sig_stick | 1);
  target += exponential_lpdf(sig_delta | 1);
  target += exponential_lpdf(sig_psi   | 1);

  //////////////////////////////////////////////////
  // // step through data row by row and define LLH
  //////////////////////////////////////////////////  
  vector[n_targets] weights;

  // some counters and index variables, etc.
  int t; // trial counter
  //////////////////////////////////////////////////
  // // step through data row by row and define LLH
  //////////////////////////////////////////////////  
  for (ii in 1:N) {

    t = trial[ii];
 
    // set the weight of each target to be its class weight
    weights = (uA[X[t], Z[ii]]) * to_vector(item_class[t]) ;

    // multiply weights by stick/switch preference
    weights = inv_logit(weights) .* inv_logit(u_stick[X[t], Z[ii]] * S[ii]); 

    weights = weights .* compute_spatial_weights(found_order[ii], n_targets, ii,
       u_delta[X[t], Z[ii]], u_psi[X[t], Z[ii]], 
       delta[ii], psi[ii], phi[ii],
       item_x[t], item_y[t]);
        
    // remove already-selected items, and standarise to sum = 1 
    weights = standarise_weights(weights, n_targets, remaining_items[ii]);   
    //print("Y ", Y[ii]);
    //print("item y ", item_y[t]);
    //print(weights);

    target += log((weights)[Y[ii]]);      
  }
}

generated quantities {
  // here we  can output our prior distritions
  real prior_bA = normal_rng(prior_mu_bA, prior_sd_bA);
  real prior_b_stick = normal_rng(prior_mu_b_stick, prior_sd_b_stick);
  real prior_rho_delta = normal_rng(prior_mu_rho_delta, prior_sd_rho_delta);
  real prior_rho_psi = normal_rng(prior_mu_rho_psi, prior_sd_rho_psi);
}