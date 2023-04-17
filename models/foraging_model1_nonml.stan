// spatial foraging project

functions{

  vector standarise_weights(vector w, int n_targets, vector remaining_items) {

    /* set weights of found items to 0 and divide by the sum of 
    remaining weights so that they sum to 1 */
    vector[n_targets] w_s = w .* remaining_items;  
    w_s = w_s / sum(w_s);
    return(w_s);
  }

  vector compute_spatial_weights(int n, int n_targets, int kk, int ii,
                                 real phi_dis, real phi_dir,
                                 vector D, vector E, vector A) {

    vector[n_targets] w;

    w = rep_vector(1, n_targets); 
     // now start computing the weights
    if (n == 1) {

      // calculate inital selection weights based on spatial location

    } else {

      if (n == 2) {
        // for the second selected target, weight by distance from the first
        w = w .* exp(-phi_dis * D);
      } else {
        // for all later targets, also weight by direciton
        w = w .* exp(-phi_dis * D - phi_dir * E);
      }
    }
    return(w);
  }
}

data {
  int <lower = 1> N; // total number of selected targets over the whole experiment
  int <lower = 1> K; // number of experimental conditions  

  int <lower = 1> n_trials;  // total number of trials (overall)
  int <lower = 1> n_classes; // number of target classes - we assume this is constant over n_trials
  int <lower = 1> n_targets; // total number of targets per trial
  array[N] int <lower = 0, upper = n_targets> trial_start; // = 1 is starting a new trial, 0 otherwise

  array[N] int <lower = 1> Y; // target IDs - which target was selected here? This is what we predict
  
  array[N] vector<lower = 0>[n_targets] D; // distance measures
  array[N] vector<lower = 0>[n_targets] E; // direction measures (relative)
  array[N] vector[n_targets] A; // direction measures (absolute)

  array[n_trials] int <lower = 1, upper = K> X; // trial features (ie, which condition are we in)
  array[n_trials, n_targets] int <lower = -1, upper = 1> targ_class; // target class, one row per trial
  array[N] vector<lower = -1, upper = 1>[n_targets] S; // stick/switch (does this targ match prev targ) 
  
  real prior_sd_bA; // param for class weight prior
  real prior_sd_bS; // prior for sd for bS
  real prior_mu_phidis;
  real prior_sd_phidis;
  real prior_mu_phidir;
  real prior_sd_phidir;
}

parameters {
  // These are all the parameters we want to fit to the data
  array[K] real bA; // weights for class A compared to B  
  array[K] real bS; // stick-switch rates 
  array[K] real phi_dis; // distance tuning
  array[K] real phi_dir; // direction tuning 
}

transformed parameters {

}

model {

  // some counters and index variables, etc.
  vector[n_targets] remaining_items; // binary vector that tracks which targets have been found
  vector[n_targets] weights;  // class weight for teach target
  vector[n_targets] m; // does this target match the previous target?
  vector[n_targets] spatial_weights;

  int trl = 0; // counter for trial number
  int kk; // condition (block) index

  /////////////////////////////////////////////////////
  // Define Priors
  ////////////////////////////////////////////////////

  //-----priors intial item selection distributions---

  // priors for fixed effects
  for (ii in 1:K) {
    target += normal_lpdf(bA[ii]      | 0, prior_sd_bA);
    target += normal_lpdf(bS[ii]      | 0, prior_sd_bS);
    target += normal_lpdf(phi_dis[ii] | prior_mu_phidis, prior_sd_phidis);
    target += normal_lpdf(phi_dir[ii] | prior_mu_phidir, prior_sd_phidir);
  }

  //////////////////////////////////////////////////
  // // step through data row by row and define LLH
  //////////////////////////////////////////////////  
  for (ii in 1:N) {

    // check if we are at the start of a new trial
    // if we are, initialise a load of things
    if (trial_start[ii] == 1) {

      trl = trl + 1; // update trial counter           
      kk = X[trl]; // get conditions of current target/trial 

      // as we're at the start of a new trial, reset the remaining_items tracker
      remaining_items = rep_vector(1, n_targets);    
    }

    // new trial, so update the class weights to take random effects into account
    // set the weight of each target to be its class weight
     
    weights = bA[kk] * to_vector(targ_class[trl]);

    // apply spatial weighting
    spatial_weights = compute_spatial_weights(trial_start[ii], n_targets, kk, ii,
                                 phi_dis[kk], phi_dir[kk], D[ii], E[ii], A[ii]);


    if (trial_start[ii] == 1) {
      weights = inv_logit(weights);
      } else {
      // check which targets match the previously selected target
      // this is precomputed in S[ii]
      weights = inv_logit(weights) .* inv_logit(bS[kk] * S[ii]); 
    }

    weights = weights .* spatial_weights;

    // remove already-selected items, and standarise to sum = 1
    weights = standarise_weights(weights, n_targets, remaining_items);

    // likelihood! 
    target += categorical_lpmf(Y[ii] | weights);
    
    // do I need this if statement? 
    if (Y[ii] == n_targets+1) {
      // trial completed
    } else {
      // remove found target from list of remaining remaining_items
      remaining_items[Y[ii]] = 0;
    }
  }
}

generated quantities {
  // here we  can output our prior distritions
  real prior_bA = normal_rng(0, prior_sd_bA);
  real prior_bS = normal_rng(0, prior_sd_bS);
  real prior_phi_dis = normal_rng(prior_mu_phidis, prior_sd_phidis);
  real prior_phi_dir = normal_rng(prior_mu_phidir, prior_sd_phidir);
}
