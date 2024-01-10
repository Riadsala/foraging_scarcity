# some helpful functions for extracting fixed and random effect posterior of our foraging model 

# cl contains the condition labels

extract_post <- function(m, d, cl) {
  
  # extract ALL parameters and collect into a list
  # to ensure that we have the same draws for each part,
  # extract everything and then filter
  
  post_fixed <- extract_post_fixed(m, d, cl)
  post_random <- extract_post_random(m, d, cl)
  
  return(list(fixed = post_fixed,
              random = post_random ))
}

extract_post_fixed <- function(m, d, cl) {

  m %>%
    spread_draws(bA[condition], b_stick[condition], rho_delta[condition], rho_psi[condition]) %>%
    select(-.chain, -.iteration) %>%
    mutate(condition = as_factor(condition)) %>%
    ungroup() -> post
  
  levels(post$condition) <- cl
  
  return(post)
}

extract_post_init_sel_fixed <- function(m) {
  
  # Now get init selection params
  gather_draws(m, init_bias_params2[k]) %>%
    ungroup() %>%
    select(-.chain, -.iteration, -.variable) %>%
    mutate(variable = case_when(
      k == 1 ~ "a_x_1",
      k == 2 ~ "b_x_1",
      k == 3 ~ "a_y_1",
      k == 4 ~ "b_y_1",
      k == 5 ~ "a_x_2",
      k == 6 ~ "b_x_2",
      k == 7 ~ "a_y_2",
      k == 8 ~ "b_y_2")) %>%
    separate(variable, into = c("param", "dim", "comp")) %>%
    select(-k) %>%
    pivot_wider( names_from = param, values_from = .value) -> post_init_sel
  
  return(post_init_sel)
  
}
 
extract_post_init_sel_prior <- function(m) {
  
  # Now get init selection params
  gather_draws(m, 
               prior_ax1, prior_bx1, prior_ay1, prior_by1,
               prior_ax2, prior_bx2, prior_ay2, prior_by2) %>%
    ungroup() %>%
    select(-.chain, -.iteration)  %>%
    mutate(.variable = str_remove(.variable, "prior_")) %>%
    separate(.variable, into = c("w", "param", "dim", "comp"), sep = "") %>%
    select(-w) %>%
    pivot_wider( names_from = param, values_from = .value) -> post_init_sel
  
  return(post_init_sel)
  
} 
extract_post_fixed_abs_dir_comps <- function(m, d, cl, n=1000) {
  
  # extracts the posterior for kappa and theta
  # these are used to model absolute directions
  
  m %>% recover_types(d) %>%
    spread_draws(theta[angle], kappa[angle], ndraws = n) %>%
    mutate(angle = (angle-1)*90,
           angle = as_factor(angle)) %>%
    ungroup() -> post
  
  return(post)
  
}

extract_post_random <- function(m, d, cl) {
  
  m %>% 
    spread_draws(uA[condition, person], 
                 u_stick[condition, person], 
                 u_delta[condition, person], 
                 u_psi[condition, person])  %>%
    select(-.chain, -.iteration) %>%
    mutate(condition = as_factor(condition)) %>%
    ungroup() -> post
  
  levels(post$condition) <- cl
  
  return(post)
  
}

extract_post_lambda <- function(m) {
  
  m %>% recover_types(d$found) %>%
    spread_draws(lambda[person]) %>%
    mutate(person = as_factor(person)) -> post_lambda
  
  return(post_lambda)
}


summarise_postpred <- function(dataset, model_version, draw_sample_frac = 0.1) {
  
  pp_folder <- paste0("../../scratch/", dataset, "/postpred/", model_version, "/")
  pp_files <- dir(pp_folder)
  
  dout <- tibble()
  
  # read in file
  ctr = 0
  for (p in pp_files) {
    ctr = ctr  +1
    pp <- read_cmdstan_csv(paste0(pp_folder, p))
    
   as_tibble(pp$generated_quantities) %>%
      sample_frac(draw_sample_frac)  %>% 
      mutate(draw = 1:n()) %>% 
     pivot_longer(-draw, names_to = "x", values_to = "y") %>%
      mutate(x = str_remove(x, "1."),
             x = str_remove(x, "]")) %>% 
      separate(x, into = c("param", "row"), convert = TRUE) %>%
      pivot_wider(names_from = "param", values_from = "y") -> pp
    
    d$found %>% mutate(row = 1:n()) %>% 
      full_join(pp, by = "row") -> pp
    
    pp %>% group_by(condition, draw, found, person) %>%
      summarise(acc = mean(P==found),
                W = mean(W), .groups = "drop_last") %>% 
      summarise(acc = mean(acc),
                W = mean(W), .groups = "drop") %>%
      mutate(model = model_version,
             chain = ctr) -> pp
    
    dout <- bind_rows(dout, pp)
  }
  
  return(dout)
  
}
