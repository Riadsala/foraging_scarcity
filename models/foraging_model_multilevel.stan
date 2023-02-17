functions{

  vector standarise_weights(vector w, int n_targets, vector remaining_items) {

    /* set weights of found items to 0 and divide by the sum of 
    remaining weights so that they sum to 1 */
    vector[n_targets] w_s = w .* remaining_items;  
    w_s = w_s / sum(w_s);
    return(w_s);
  }

  vector compute_spatial_weights(int n, int n_targets, int kk, int ll, int ii,
                                 real phi_dis, real phi_dir, 
                                 matrix u, vector D, vector E, vector A, 
                                 vector itemX, vector itemY) {

    vector[n_targets] w;

    w = rep_vector(1, n_targets); 
     // now start computing the weights
    if (n == 1) {

      // calculate inital selection weights based on spatial location

    } else {

      if (n == 2) {
        // for the second selected target, weight by distance from the first
        w = w .* exp(-(phi_dis + u[3+4*(kk-1), ll]) * D);
      } else {
        // for all later targets, also weight by direciton
       w = w .* exp(-(phi_dis + u[3+4*(kk-1), ll]) * D - (phi_dir + u[4+4*(kk-1), ll]) * E);
      }
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
  int <lower = 0, upper = n_targets> trial_start[N]; // = 1 is starting a new trial, 0 otherwise

  int <lower = 1> Y[N]; // target IDs - which target was selected here? This is what we predict
  
  vector[n_targets] itemX[n_trials]; // x postiions of each target in each trial
  vector[n_targets] itemY[n_trials]; // y position
  vector<lower = 0>[n_targets] D[N]; // distance measures
  vector<lower = 0>[n_targets] E[N]; // direction measures (relative)
  vector[n_targets] A[N]; // direction measures (absolute)

  int <lower = 1, upper = K> X[n_trials]; // trial features (ie, which condition are we in)
  int <lower = 1, upper = n_classes> targ_class[n_trials, n_targets]; // target class, one row per trial
  vector<lower = -1, upper = 1>[n_targets] S[N]; // stick/switch (does this targ match prev targ) 
  int <lower = 1, upper = L> Z[N]; // random effect levels 
  
  real prior_sd_bAvP; // param for class weight prior
  real prior_sd_bS; // prior for sd for bS
  real prior_mu_phidis;
  real prior_sd_phidis;
  real prior_mu_phidir;
  real prior_sd_phidir;
}

parameters {
  // These are all the parameters we want to fit to the data

  ////////////////////////////////////
  // fixed effects
  ////////////////////////////////////

  /* in order to allow for correlations between the
  variables, these are all stored in a list
  these include bAvB, bS (stick weight), and the two spatial 
  sigmas, along with the floor (chance of selectin an 
  item at random)
  */
  real b[4*K];

  ///////////////////////////////
  // random effects
  ///////////////////////////////
 
  vector<lower=0>[4*K] sig_b; // random effect sigma for biases  
  cholesky_factor_corr[4*K] L_u; // declare L_u to be the Choleski factor of a correlation matrix
  matrix[4*K,L] z_u;  // random effect matrix
}

transformed parameters {

  // extract params from list of params    
  real bAvB[K]; // weights for class A compared to B  
  real bS[K]; // stick-switch rates 
  real phi_dis[K]; // distance tuning
  real phi_dir[K]; // direction tuning

  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[4*K,L] u;
  u = diag_pre_multiply(sig_b, L_u) * z_u; 

  // extract params from list of params
  for (ii in 1:K) {
    bAvB[ii]    = b[1+4*(ii-1)];
    bS[ii]      = b[2+4*(ii-1)];
    phi_dis[ii] = b[3+4*(ii-1)];
    phi_dir[ii] = b[4+4*(ii-1)];
  }
}

model {

  // some counters and index variables, etc.
  vector[n_targets] remaining_items; // binary vector that tracks which targets have been found
  vector[n_targets] weights;  // class weight for teach target
  vector[n_targets] m; // does this target match the previous target?
  vector[n_classes] ucW; // class weights with random effects included
  vector[n_targets] spatial_weights;

  int ll; // participant tracker
  int trl = 0; // counter for trial number
  int kk; // condition (block) index

  /////////////////////////////////////////////////////
  // Define Priors
  ////////////////////////////////////////////////////

  //-----priors intial item selection distributions---

  // priors for fixed effects
  for (ii in 1:K) {
    target += normal_lpdf(b[1+4*(ii-1)] | 0, prior_sd_bAvP);
    target += normal_lpdf(b[2+4*(ii-1)] | 0, prior_sd_bS);
    target += normal_lpdf(b[3+4*(ii-1)] | prior_mu_phidis, prior_sd_phidis);
    target += normal_lpdf(b[4+4*(ii-1)] | prior_mu_phidir, prior_sd_phidir);
  }

  //priors for random effects (use covariance matrix)
  sig_b ~ normal(0, 1);
  L_u ~ lkj_corr_cholesky(1.5); // LKJ prior for the correlation matrix
  to_vector(z_u) ~ normal(0, 1);

  //////////////////////////////////////////////////
  // // step through data row by row and define LLH
  //////////////////////////////////////////////////  
  for (ii in 1:N) {

    // check if we are at the start of a new trial
    // if we are, initialise a load of things
    if (trial_start[ii] == 1) {

      ll = Z[ii]; // get observer level for this trial
      trl = trl + 1; // update trial counter           
      kk = X[trl]; // get conditions of current target/trial 

      // as we're at the start of a new trial, reset the remaining_items tracker
      remaining_items = rep_vector(1, n_targets);

      // new trial, so update the class weights to take random effects into account
      ucW[1] = inv_logit(bAvB[kk] + u[1+4*(kk-1), ll]);
      ucW[2] = 1 - ucW[1];
    }

    // apply spatial weighting
    spatial_weights = compute_spatial_weights(trial_start[ii], n_targets, kk, ll, ii,
                                 phi_dis[kk], phi_dir[kk], u, D[ii], E[ii], A[ii],
                                 itemX[trl], itemY[trl]);

    // set the weight of each target to be its class weight
    weights = ucW[targ_class[trl]] .* spatial_weights;

    if (trial_start[ii] > 1) {
      // check which targets match the previously selected target
      // this is precomputed in S[ii]
      weights = weights .* inv_logit((bS[kk] + u[2+4*(kk-1), ll]) * S[ii]); 
    }

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
  real prior_cW = normal_rng(0, prior_sd_bAvP);
  real prior_sW = normal_rng(0, prior_sd_bS);
  real prior_phi_dis = normal_rng(prior_mu_phidis, prior_sd_phidis);
  real prior_phi_dir = normal_rng(prior_mu_phidir, prior_sd_phidir);
  real prior_direction_bias = normal_rng(-2, 3);
}