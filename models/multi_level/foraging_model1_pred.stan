functions{

  vector standarise_weights(vector w, int n_targets, vector remaining_items) {

    /* set weights of found items to 0 and divide by the sum of 
    remaining weights so that they sum to 1 */
    vector[n_targets] w_s = w .* remaining_items;  

    w_s = w_s / sum(w_s);

    return(w_s);
  }

  vector init_spat_bias(int n_targets, vector x, vector y, vector ab, real lambda) {

    vector[n_targets] w, w1, w2;

    /* for initial selection we want to weight each item by
    how likely it is to come from a two component beta 
    mixture model */
    for (ii in 1:n_targets) {

      w1[ii] = beta_lpdf(x[ii] | ab[1], ab[2])
        + beta_lpdf(y[ii] | ab[3], ab[4]);

      w2[ii] = beta_lpdf(x[ii] | ab[5], ab[6])
        + beta_lpdf(y[ii] | ab[7], ab[8]);
    }

    w = lambda * exp(w1) + (1-lambda) * exp(w2);

    return(w);
  }

  vector compute_spatial_weights(
    int n, int n_targets, int ii,
    real u_delta, real u_psi, vector delta, vector psi, vector phi,
    vector x, vector y, vector init_bias_params, real lambda) {

    // functions for calculating the spatial weights

    vector[n_targets] w;

    w = rep_vector(1, n_targets); 

    // now start computing the weights
    if (n == 1) {

      // calculate initial selection weights based on spatial location
      w = init_spat_bias(n_targets, x, y, init_bias_params, lambda);

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
  real prior_mu_lambda;
  real prior_sd_lambda;
  real prior_mu_ax1;
  real prior_sd_ax1;
  real prior_mu_bx1;
  real prior_sd_bx1;
  real prior_mu_ay1;
  real prior_sd_ay1;
  real prior_mu_by1;
  real prior_sd_by1;
  real prior_mu_ax2;
  real prior_sd_ax2;
  real prior_mu_bx2;
  real prior_sd_bx2;
  real prior_mu_ay2;
  real prior_sd_ay2;
  real prior_mu_by2;
  real prior_sd_by2;

  //int rel_distance;
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

  array[K] vector[L] uA; // weights for class A compared to B  
  array[K] vector[L] u_stick; // stick-switch rates 
  array[K] vector[L] u_delta; // distance tuning
  array[K] vector[L] u_psi; // direction tuning

  
  real<lower = 0> sig_a;
  real<lower = 0> sig_stick;
  real<lower = 0> sig_delta;
  real<lower = 0> sig_psi;
 
  
  ///////////////////////////////
  // initial bais parameters
  ///////////////////////////////

  /* These are constant over participants so 
  should not be included in the random effect structure

  order of params:
  a, b for comp 1, x dimension
  a, b for comp 1, y dimension
  a, b for comp 2, x dimension
  a, b for comp 2, y dimension
  */
  vector<lower = -5, upper = 5>[8] init_bias_params;

  /* lambda varies to person to person, 
  so may want to have it correlated (potentially)
  with the b params.*/
  array[L] real<lower=0, upper=1> lambda;
}

transformed parameters {

  vector[8] init_bias_params2; // exp transform
  init_bias_params2 = exp(init_bias_params);

  // combine fixed and random effects
  array[K] vector[L] zA; 
  array[K] vector[L] z_stick; 
  array[K] vector[L] z_delta; 
  array[K] vector[L] z_psi; 

  for (kk in 1:K) {
    zA[kk] = bA[kk] + uA[kk];
    z_stick[kk] = b_stick[kk] + u_stick[kk];
    z_delta[kk] = rho_delta[kk] + u_delta[kk];
    z_psi[kk]   = rho_psi[kk] + u_psi[kk];
  }
}

model {

}

generated quantities {

  /* This code steps through the data item selection by item selection and
  computes
  P: a sampled next target using the model's weights
  W: the weight the model assigned to the target that was selected next

  We will also allow the model to simulate a whole trial start-to-finish so that 
  we can compare run statistics, inter-target selection dynamics etc/
  This will be saved in Q. 
  */
        
  array[N] int P;
  array[N] real W;
  //array[n_trials * n_targets] int Q; // total number of targets may be more than N
  //array[n_trials * n_targets] int Qperson, Qtrial;//

  // first, step through data and compare model selections to human participants
  {
    // some counters and index variables, etc.
    vector[n_targets] weights;  // class weight for teach target
    int t = 0; // counter for trial number
  
    //////////////////////////////////////////////////
    // // step through data row by row and define LLH
    //////////////////////////////////////////////////  
   for (ii in 1:N) {

      t = trial[ii];
   
      // set the weight of each target to be its class weight
      weights = (zA[X[t], Z[ii]]) * to_vector(item_class[t]);

      // multiply weights by stick/switch preference
      weights = inv_logit(weights) .* inv_logit(z_stick[X[t], Z[ii]] * S[ii]); 

      weights = weights .* compute_spatial_weights(found_order[ii], n_targets, ii,
         z_delta[X[t], Z[ii]], z_psi[X[t], Z[ii]], 
         delta[ii], psi[ii], phi[ii],
         item_x[t], item_y[t], init_bias_params2, lambda[Z[ii]]);
          
      // remove already-selected items, and standarise to sum = 1 
      weights = standarise_weights(weights, n_targets, remaining_items[ii]);   

        P[ii] = categorical_rng(weights);
        W[ii] = weights[Y[ii]];
       
    }
  }

/*  // now allow the model to do a whole trial on its own
  {
    vector[n_targets] remaining_items2;
    vector[n_targets] Sj;
    // some counters and index variables, etc.
    vector[n_targets] weights;  // class weight for teach target
    int t = 0; // counter for trial number
    vector[n_targets] psi_j, phi_j, delta_j;
    int ctr;
    
    ctr = 0;
    // step through each row of data and look for each time we start a new trial
    for (ii in 1:N) {
      if (found_order[ii] == 1) {

        // init a new trial
        t = trial[ii];
        Qtrial = t;
        Qperson = Z[ii];
        remaining_items2 = rep_vector(1, n_targets);

        for (jj in 1:n_targets) {

          ctr = ctr + 1;

          // set the weight of each target to be its class weight
          weights = (zA[X[t], Z[ii]]) * to_vector(item_class[t]);

          // multiply weights by stick/switch preference

          // first we need to calcualte Sj, delta, psi and phi
          Sj = rep_vector(0, n_targets);
          psi_j   = rep_vector(1, n_targets); // not yet implemented properly
          phi_j   = rep_vector(1, n_targets); // not yet implemented properly
          delta_j = rep_vector(1, n_targets);

          if (jj > 1) {
            // compute delta
            delta_j = (item_x[t][Q[jj-1]] - item_x[t])^2 + (item_y[t][Q[jj-1]] - item_y[t])^2;
            delta_j = sqrt(delta_j);

            // compute S
            for (tt in 1:n_targets) {
              if (item_class[t][tt] == Q[jj-1]) {
                Sj[tt] = 1;
              } else {
                Sj[tt] = -1;
              }
            }
          }
          weights = inv_logit(weights) .* inv_logit(z_stick[X[t], Z[ii]] * S[ii]); 

          // now compute delta phi and psi

          weights = weights .* compute_spatial_weights(found_order[ii], n_targets, ii,
               z_delta[X[t], Z[ii]], z_psi[X[t], Z[ii]], 
               delta_j, psi_j, phi_j,
               item_x[t], item_y[t], init_bias_params2, lambda[Z[ii]]);
                
          // remove already-selected items, and standarise to sum = 1 
          weights = standarise_weights(weights, n_targets, remaining_items[ii]);   

          Q[ctr] = categorical_rng(weights);

          // update remaining_items2
          remaining_items2[Q[jj]] = 0;

        }
      }
    }
  }*/
}
    