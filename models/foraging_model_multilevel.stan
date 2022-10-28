functions{

  /* this function calculates the weights for each item based on the
     intial spatial weighting bias */   
  vector beta_weight(vector X, real a, real b) {

    int n = size(X);
    vector[n] Z;
    Z = X^(a-1).*(1-X)^(b-1);
    Z = Z / beta(a, b); 
    return(Z);
  }

  /* this function calculates the weights for each item based on the
     intial spatial weighting bias */
  vector init_sel_weights(real lbda, 
                            real ax1, real bx1, real ay1, real by1, 
                            real ax2, real bx2, real ay2, real by2, 
                            vector X, vector Y) {

    int n = size(X);
    vector[n] w1;
    vector[n] w2;
    
    // set the weights of each target based on intial-bias model
    w1 = beta_weight(X, ax1, bx1) .* beta_weight(Y, ay1, by1);
    w2 = beta_weight(X, ax2, bx2) .* beta_weight(Y, ay2, by2);

    return lbda * w1 + (1-lbda) * w2 ;
  }

  vector standarise_weights(vector w, int n_targets, vector remaining_items) {
    // set weights for found targets to 0 
    vector[n_targets] w_s = w .* remaining_items;    
    // normalise so that weights sum to 1
    w_s = w_s / sum(w_s);

    return w_s;
  }

  vector compute_spatial_weights(int n, int n_targets, int kk, int ll, int ii,
                                 real phi_dis, real phi_dir, real p_floor, real dir_bias,
                                 matrix u, vector D, vector E, vector A, real lambda,
                                 real[] a_x, real[] b_x, real[] a_y, real[] b_y,
                                 vector itemX, vector itemY) {

    vector[n_targets] w;

    w = rep_vector(1, n_targets); 
     // now start computing the weights
    if (n == 1) {

      // calculate inital selection weights based on spatial location
      
      w = init_sel_weights(lambda, 
        a_x[1], b_x[1], a_y[1], b_y[1],
        a_x[2], b_x[2], a_y[2], b_y[2], 
        itemX, itemY); 

    } else {

      if (n == 2) {
        // for the second selected target, weight by distance from the first
       w = w .* exp(-(phi_dis + u[1+3*(kk-1), ll]) * D) .* (1 + dir_bias*cos(4*A))/(dir_bias+1);
      } else {
        // for all later targets, also weight by direciton
        w = w .* exp(-(phi_dis + u[1+3*(kk-1), ll]) * D - (phi_dir + u[2+3*(kk-1), ll]) * E) .* (1 + dir_bias*cos(4*A))/(dir_bias+1);
      
    }
     // apply shelf..
    // ****** where should this operation go? unclear to me
    w = w + exp(p_floor + u[3+3*(kk-1), ll]);

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
  
  real alpha; // param for class weight prior
  real prior_sd_bS; // prior for sd for bS
  real prior_mu_phidis;
  real prior_sd_phidis;
  real prior_mu_phidir;
  real prior_sd_phidir;
  real prior_mu_floor;
  real prior_sd_floor;
}

parameters {
  // These are all the parameters we want to fit to the data

  ////////////////////////////////////
  // fixed effects
  ////////////////////////////////////

  //-----initial item selection-----//

  /* first define the a and b params for the two x and y Beta 
  distributions that we will use to descibe the intial target
  selections */
  real<lower=0> a_x[2];
  real<lower=0> a_y[2];
 
  real<lower=0> b_x[2];
  real<lower=0> b_y[2];

  // lambda is the mixing parameter and varies between people
  real<lower=0, upper=1> lambda[L];

  //-----params for foraging-----//

  // weights for each target class per condition.
  simplex[n_classes] cW[K]; 

  // stick-switch rates
  real bS[K]; 

  /* in order to allow for correlations between the
  remaining variables, these are all stored in a list
  these include bS (stick weight), and the two spatial 
  sigmas, along with the floor (chance of selectin an 
  item at random)
  */
  real b[4*K];

  ///////////////////////////////
  // random effects
  ///////////////////////////////

  // first of all, random effects for the class weights
  matrix<lower = 0>[n_classes, K] sig_cw; // random effect sigma for class weights 
  matrix[n_classes, K*L] u_cw;

  // now random effects for stick/switch rates
  real<lower = 0> sig_switch;
  matrix[K, L] u_stick;

  /* and now the other parameters 
  proximity weighting, momemtum, and the "floor" parameter
  We will model the correlations between these parameters */
  vector<lower=0>[4*K] sig_b; // random effect sigma for biases  
  cholesky_factor_corr[4*K] L_u; // declare L_u to be the Choleski factor of a correlation matrix
  matrix[4*K,L] z_u;  // random effect matrix
}

transformed parameters {

  // extract params from list of params
  real phi_dis[K]; // distance tuning
  real phi_dir[K]; // direction tuning
  real p_floor[K]; // probabiltiy floor
  real direction_bias[K]; // prefer hori-vert over oblique

  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[3*K,L] u;
  u = diag_pre_multiply(sig_b, L_u) * z_u; 

  // extract params from list of params
  for (ii in 1:K) {
    phi_dis[ii] = b[1+4*(ii-1)];
    phi_dir[ii] = b[2+4*(ii-1)];
    p_floor[ii] = b[3+4*(ii-1)];
    direction_bias[ii] = inv_logit(b[4+4*(ii-1)]);
  }
}

model {

  // some counters and index variables, etc.
  vector[n_targets] remaining_items; // binary vector that tracks which targets have been found
  vector[n_targets] weights;  // class weight for teach target
  vector[n_targets] m; // does this target match the previous target?
  vector[n_classes] ucW; // class weights with random effects included

  int ll; // participant tracker
  int trl = 0; // counter for trial number
  int kk; // condition (block) index
  real quit_weight = 0;

  /////////////////////////////////////////////////////
  // Define Priors
  ////////////////////////////////////////////////////

  //-----priors intial item selection distributions---

  /* Based on Clarke et al (2022, Vision), we will set
  informative priors for top left corner and centre */
  a_x[1] ~ normal(1.0, 0.10);
  b_x[1] ~ normal(8.0, 0.50);

  a_y[1] ~ normal(1.0, 0.10);
  b_y[1] ~ normal(8.0, 0.50);

  a_x[2] ~ normal(1.5, 0.25);
  b_x[2] ~ normal(1.5, 0.25);

  a_y[2] ~ normal(1.5, 0.25);
  b_y[2] ~ normal(1.5, 0.25);

  lambda ~ beta(0.85, 0.9);

  // priors for fixed effects
  for (ii in 1:K) {
    target += dirichlet_lpdf(cW[ii] |  rep_vector(alpha, n_classes));
    target += normal_lpdf(b[1+4*(ii-1)] | prior_mu_phidis, prior_sd_phidis);
    target += normal_lpdf(b[2+4*(ii-1)] | prior_mu_phidir, prior_sd_phidir);
    target += normal_lpdf(b[3+4*(ii-1)] | prior_mu_floor, prior_sd_floor);
    target += normal_lpdf(b[4+4*(ii-1)] | 0, 2);
  }

  // priors for random effects - class weights
  for (ii in 1:K) {
    for (jj in 1:n_classes) {
      target += normal_lpdf(sig_cw[jj, ii] | 0, 0.50);
      for (obs in 1:L) {
        u_cw[jj, ii + (obs-1)*K] ~ normal(0, sig_cw[jj, ii]);
      }
    }
  }

  // priors for random effects - stick/switch weights
  sig_switch ~ normal(0, 0.1);
  for (ii in 1:K) { 
    target += normal_lpdf(bS[ii] | 0, prior_sd_bS);
    for (obs in 1:L) {
      u_stick[ii, obs] ~ normal(0, sig_switch);
    }
  }
  
  // spatial parameters (use covariance matrix)
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
      ucW = inv_logit(logit(cW[kk]) + u_cw[1:n_classes, kk + (ll-1)*K]);
      ucW = ucW / sum(ucW);
    }

    // apply spatial weighting
    vector[n_targets] spatial_weights = compute_spatial_weights(trial_start[ii], n_targets, kk, ll, ii,
                                 phi_dis[kk], phi_dir[kk], p_floor[kk], direction_bias[kk], u, D[ii], E[ii], A[ii],
                                 lambda[ll], a_x, b_x, a_y, b_y, itemX[trl], itemY[trl]);

    // set the weight of each target to be its class weight
    weights = ucW[targ_class[trl]] .* spatial_weights;

    if (trial_start[ii]>1) {
      // check which targets match the previously selected target
      // this is precomputed in S[ii]
      weights = weights .* inv_logit((bS[kk] + u_stick[kk, ll]) * S[ii]); 
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
  simplex[n_classes] prior_cW = dirichlet_rng( rep_vector(alpha, n_classes));
  real prior_sW = normal_rng(0, prior_sd_bS);
  real prior_phi_dis = normal_rng(prior_mu_phidis, prior_sd_phidis);
  real prior_phi_dir = normal_rng(prior_mu_phidir, prior_sd_phidir);
  real prior_phi_flr = normal_rng(prior_mu_floor, prior_sd_floor);
  real prior_direction_bias = beta_rng(1, 1);
}