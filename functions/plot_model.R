library(tidybayes)
library(patchwork)

# plotting our foraging model

# plot_model_fixed() plots the fixed effects (group averages) 
# plot_post_prior() - why is this called this? 
# plot_traceplots(m) plots the traceplots for the core params in our model 
# plot_beta_comp() details needed
# plot_init_sel() - plots the inital selection component of the model

# plot_init_sel() needs a tidy up?
# MORE sample_beta() to post_functions ???
# plot_model_spatial () will need fixed at some point


options(ggplot2.discrete.colour = ggthemes::ptol_pal()(4),
        ggplot2.discrete.fill = ggthemes::ptol_pal()(4))

plot_model_fixed <- function(post, m, d, cl = "A", gt=NULL, directions = FALSE){
  
  # gt is a list with our groundtruth sim parameters. 
  post <- post$fixed
  
  m %>% recover_types(df) %>%
    spread_draws(prior_bA, prior_b_stick, prior_rho_delta, prior_rho_psi) %>%
    ungroup() -> prior
  
  my_widths <- c(0.53, 0.97)
  
  # for real data
  if (is.null(gt)) {
  # plot class weights
  post %>%
    mutate(
      difficulty = case_match(
        difficulty,
        "hard" ~ "conjunction",
        "easy" ~ "feature")) %>%
    ggplot() + 
    geom_rect(data = prior %>% 
                median_hdci(prior_bA, .width = c(0.53, 0.97)) %>%
                mutate(.lower = plogis(.lower), .upper = plogis(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "orange", alpha = 0.25) +
    geom_density(aes(plogis(bA), fill = condition), alpha = 0.5) +
    geom_vline(xintercept = 0.5, linetype = 2) +
    scale_x_continuous("class weights") +
    facet_wrap(~difficulty) + 
    theme(legend.position = "bottom") -> plt_pA
  
  # plot difference between class weights
    post %>% 
      pivot_longer(c(bA, b_stick, rho_delta, rho_psi), names_to = "param") %>%
      pivot_wider(names_from = c("condition", "difficulty"), values_from = "value") %>%
      unnest() %>%
      mutate(feature = scarce_easy - equal_easy,
             conjunction = scarce_hard - equal_hard) %>%
      pivot_longer(c(feature, conjunction), names_to = "difficulty") -> post_diff
    
    post_diff %>%
      filter(param == "bA") %>%
      ggplot(aes(value)) + geom_density(fill = "grey") + 
      geom_vline(xintercept = 0, linetype = 2) + 
      facet_wrap(~difficulty, scales="free", nrow = 1) +
      theme(legend.position = "none", text = element_text(size=10)) + xlab('class weight difference between equal & scarce conditions') -> plt_pA_diff
  }
  
  # for sim data
  if (!is.null(gt)) {
    # plot class weights
    post %>%
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(prior_bA, .width = c(0.53, 0.97)) %>%
                  mutate(.lower = plogis(.lower), .upper = plogis(.upper)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "orange", alpha = 0.25) +
      geom_density(aes(plogis(bA), fill = condition), alpha = 0.5) +
      geom_vline(xintercept = 0.5, linetype = 2) +
      scale_x_continuous("class weights") +
      theme(legend.position = "bottom") -> plt_pA
  }
    
  
  # for real data
  if (is.null(gt)) {
  # plot stick-switch param
  post  %>%
    mutate(
      difficulty = case_match(
        difficulty,
        "hard" ~ "conjunction",
        "easy" ~ "feature")) %>%
    ggplot()  + 
    geom_rect(data = prior %>% 
                median_hdci(prior_b_stick, .width = c(0.53, 0.97)) %>%
                mutate(.lower = plogis(.lower), .upper = plogis(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "orange", alpha = 0.25) + 
    geom_density(aes(plogis(b_stick), fill = condition), alpha = 0.5) +
    geom_vline(xintercept = 0.5, linetype = 2) +
    scale_x_continuous("stick probability")  +
    facet_wrap(~difficulty) + 
    theme(legend.position = "bottom") -> plt_pS
    
  }
  
  # for sim data
  if (!is.null(gt)) {
    # plot stick-switch param
    post  %>%
      ggplot()  + 
      geom_rect(data = prior %>% 
                  median_hdci(prior_b_stick, .width = c(0.53, 0.97)) %>%
                  mutate(.lower = plogis(.lower), .upper = plogis(.upper)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "orange", alpha = 0.25) + 
      geom_density(aes(plogis(b_stick), fill = condition), alpha = 0.5) +
      geom_vline(xintercept = 0.5, linetype = 2) +
      scale_x_continuous("stick probability")  +
      theme(legend.position = "bottom") -> plt_pS
    
  }
  
  
  # plot proximity and direction effects
  plt_prox    <- plt_post_prior(post, prior, "rho_delta", "proximity tuning", gt$rho_delta)
  plt_rel_dir <- plt_post_prior(post, prior, "rho_psi", "direction tuning", gt$rho_psi)
  
  
  if (!is.null(gt)) {
    # if we have groundtruth, annotate our plt
    plt_pA + geom_vline(xintercept = gt$pA, linetype = 1, colour = "red") -> plt_pA
    
    plt_pS + geom_vline(xintercept = plogis(gt$b_stick), linetype= 1, colour = "red") -> plt_pS
  }
  
  # add direction plot, if we're using that model
  if (directions) {
    m %>% recover_types(d) %>%
      spread_draws(theta[angle]) %>%
      mutate(angle = (angle-1)*90,
             angle = as_factor(angle)) %>%
      ungroup() %>%
      ggplot(aes(theta, fill = angle)) + 
      geom_density(alpha = 0.5) + 
      geom_vline(data =  tibble(angle = as_factor(0:1 * 90), theta = gt$theta), 
                 aes(xintercept = theta, colour = angle), linetype = 1) + 
      ggthemes::scale_color_ptol() -> plt_abs_dir_theta
    
    m %>% recover_types(d) %>%
      spread_draws(kappa[angle]) %>%
      mutate(angle = (angle-1)*90,
             angle = as_factor(angle)) %>%
      ungroup() %>%
      ggplot(aes(kappa, fill = angle)) + 
      geom_density(alpha = 0.5) + 
      geom_vline(data = tibble(angle = as_factor(0:1 * 90), kappa = gt$kappa), 
                 aes(xintercept = kappa, colour = angle), linetype = 1) + 
      ggthemes::scale_color_ptol() -> plt_abs_dir_kappa
    
    post_dir <- extract_post_fixed_abs_dir_comps(fit, d, n = 100) 
    
    map_df(unique(post_dir$.draw), compute_rho_phi) %>%
      ggplot(aes(x = phi, y = rho)) + geom_path(aes( group = .draw), alpha = 0.25) + 
      geom_path(data = ddir, aes(phi, z), colour = "darkred", linewidth = 2 ) -> plt_dir_dist
    
    plt <- plt / (plt_abs_dir_theta + plt_abs_dir_kappa + plt_dir_dist + plot_layout(widths = c(1,1,2)))
  }
  
  # for real data
  if (is.null(gt)) {
  layout <- "
  AAAABBBB
  CCCCDDEE
  "
  
  plt <- (plt_pA + plt_pA_diff + plt_pS + plt_prox + plt_rel_dir) + 
    plot_layout(design = layout, guides = 'collect') &  
    theme(legend.position = 'bottom', panel.grid = element_blank())
  
  return(plt)
  }
  
  # for sim data
  if (!is.null(gt)){
    plt <- plt_pA + plt_pS + plt_prox + plt_rel_dir
    
    plt <- plt +
      plot_layout(guides = "collect")  & theme(legend.position = 'bottom')
    
  }
  
}
  
compute_rho_phi <- function(drw) {
  
  p <- filter(post_dir, .draw == drw)
  
  theta <- c(p$theta[1], p$theta[2], p$theta[1], p$theta[2])
  kappa <- c(p$kappa[1], p$kappa[2], p$kappa[1], p$kappa[2])
  phi = seq(1, 360)
  
  ddir <- tibble(phi = seq(1, 360),
                 .draw = drw,
                 rho = compute_all_von_mises(theta, kappa, phi))
  
  return(ddir)
}



plt_post_prior <- function(post, prior, var, xtitle, gt) {
  
  # function to plot the posterior against the prior. 
  # gt allows us to mark up the groundtruth (if available)
  
  prior_var = paste("prior", var, sep = "_")
  
  if (var == "p_floor") 
  {
    post %>% 
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(exp(get(prior_var)), .width = c(0.53, 0.97)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "orange", alpha = 0.25) +  
      geom_density(aes(exp(get(var)), fill = condition), alpha = 0.5) +
      scale_x_continuous(xtitle) +
      coord_cartesian(xlim = c(0, 0.1)) -> plt
    
  } else {
    post %>% 
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(get(prior_var), .width = c(0.53, 0.97)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "orange", alpha = 0.25) +  
      geom_density(aes(get(var), fill = condition), alpha = 0.5) +
      geom_vline(xintercept = gt, linetype = 1, colour = "red") +
      scale_x_continuous(xtitle) -> plt
    
  }
  
  return(plt)
  
}

plot_traceplots <- function(m)
{
  
  bayesplot::mcmc_trace(m$draws(), pars = c("bA", "b_stick"))
  
}

plt_beta_comp <- function(dat, var) {
  
  dat %>% filter(dim == var) %>%
    ggplot(aes(x, z, group = interaction(c, draw), colour = c)) +
    geom_path(alpha = 0.1) + 
    theme(legend.position = "none")  + 
    scale_y_continuous("likelihood")  -> plt
  
  if (var == "y") {
    plt <- plt + coord_flip() + 
      scale_x_reverse()  +
      xlab(var)
  }
  
  return(plt)
  
}

sample_beta <- function(.draw, dim, comp, a, b) {
  
  x <- seq(0.01, 0.99, 0.01)
  
  return(tibble(c = comp, 
                draw = .draw,
                dim = dim,
                x = x,
                z = dbeta(x, shape1 = a, shape2 = b)))
}

plot_init_sel <- function(post, d, pp=FALSE) {
  
  post$lambda %>%
    mutate(person = as_factor(person)) %>%
    ggplot(aes(lambda, person)) +
    stat_pointinterval(colour = "purple") -> plt_lambda
  
  if (pp==FALSE) {
  post$init_sel %>%
    sample_n(500) %>%
      pmap_df(sample_beta) %>%
      mutate(c = as_factor(c)) -> plt_dat
    
  } else {
    post$init_prior %>%
      sample_n(500) %>%
      mutate(a = a, 
             b = b) %>%
      pmap_df(sample_beta) %>%
      mutate(c = as_factor(c)) -> plt_dat
    
  }
   
    plt <- plt_lambda + plt_beta_comp(plt_dat, "y") + plt_beta_comp(plt_dat, "x")
   
    return(plt)
}
