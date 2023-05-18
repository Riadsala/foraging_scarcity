library(tidybayes)

options(ggplot2.discrete.colour = ggthemes::ptol_pal()(4),
        ggplot2.discrete.fill = ggthemes::ptol_pal()(4))

plot_model_fixed <- function(m, d, cl, gt=NULL, merge_conditions=FALSE, fix_priorNames = FALSE)
{
  # gt is a list with our groundtruth sim parameters. 
  
  if (is.null(gt)) { # i.e. experimental
    
    m %>% recover_types(d) %>%
      spread_draws(bA[condition], bS[condition], phi_dis[condition], phi_dir[condition]) %>%
      mutate(condition = as_factor(condition)) %>%
      ungroup() -> post
    
    post %>% 
      separate(condition, c("difficulty", "condition")) -> post
    
  } else { # i.e. simulated data
    
    m %>% recover_types(d) %>%
      spread_draws(bA[condition], bS[condition], phi_dis[condition], phi_dir[condition]) %>%
      mutate(condition = as_factor(condition)) %>%
      mutate(condition = fct_recode(condition, !!!cl)) %>%
      ungroup() -> post
    
  }
  
 if (merge_conditions) {
   post %>%
     mutate(bA = if_else(condition == "A", -bA, bA),
       condition = if_else(condition == "AB", "equal", "scarce")) -> post
   
 }
  
  
  if (fix_priorNames == FALSE) {
  m %>% recover_types(d$found) %>%
    spread_draws(prior_bA, prior_bS, prior_phi_dis, prior_phi_dir) %>%
    ungroup() -> prior 
  } else {
    m %>% recover_types(d$found) %>%
      spread_draws(prior_cW, prior_sW, prior_phi_dis, prior_phi_dir) %>%
      ungroup() %>%
      rename(prior_bA = "prior_cW", prior_bS = "prior_sW") -> prior 
    
    
  }
  
  my_widths <- c(0.53, 0.97)
  
 
  # plot class weights
  post %>%
    ggplot() + 
    geom_rect(data = prior %>% 
                median_hdci(prior_bA, .width = c(0.53, 0.97)) %>%
              mutate(.lower = boot::inv.logit(.lower),
                     .upper = boot::inv.logit(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "lightgrey", alpha = 0.5) +
    geom_density(aes(boot::inv.logit(bA), fill = condition), alpha = 0.5) +
    scale_x_continuous("class weights", limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1")) +
    theme(legend.position = "none") +
    geom_vline(xintercept = 0.5, linetype = 2) -> plt_bA
  
  # plot difference between class weights
  
  if (is.null(gt)) {
  post %>% 
    pivot_longer(c(bA, bS, phi_dis, phi_dir), names_to = "param") %>%
    pivot_wider(names_from = c("condition", "difficulty"), values_from = "value") %>%
    unnest() %>%
    mutate(easy = scarce_easy - equal_easy,
           hard = scarce_hard - equal_hard) %>%
    pivot_longer(c(easy, hard), names_to = "difficulty") -> post_diff
  
  post_diff %>%
    filter(param == "bA") %>%
    ggplot(aes(value)) + geom_density(fill = "grey") + 
    geom_vline(xintercept = 0, linetype = 2) + 
    facet_wrap(~difficulty, scales="free", nrow = 1) +
    theme(legend.position = "none") + xlab('class weight difference between equal and scarce conditions') -> plt_pA_diff
  
  }
  
  # plot stick-switch param
  post  %>%
    ggplot() + 
    geom_rect(data = prior %>% 
                median_hdci(prior_bS, .width = c(0.53, 0.97)) %>%
                mutate(.lower = boot::inv.logit(.lower),
                       .upper = boot::inv.logit(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "lightgrey", alpha = 0.5) + 
    geom_density(aes(plogis(bS), fill = condition), alpha = 0.5) +
    scale_x_continuous("stick probability", limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "0.25", "0.50", "0.75", "1"))  +
    geom_vline(xintercept = 0.5, linetype = 2)-> plt_bS
  
  # plot proximity and direction effects
  plt_dis <- plt_post_prior(post, prior, "phi_dis", "proximity tuning", gt$sig_d)
  plt_dir <- plt_post_prior(post, prior, "phi_dir", "direction tuning", gt$sig_theta)
 #
  if (merge_conditions) {
    plt_bA <- plt_bA + facet_wrap(~difficulty)
    plt_bS <- plt_bS + facet_wrap(~difficulty)
    plt_dis <- plt_dis + facet_wrap(~difficulty)
    plt_dir <- plt_dir + facet_wrap(~difficulty)
  }
  
  
  if (!is.null(gt)) {
    # if we have groundtruth, annotate our plt
    plt_bA + geom_vline(xintercept = plogis(gt$bA), linetype = 2) -> plt_bA
    
    plt_bS + geom_vline(xintercept = plogis(gt$bS), 
                        colour = "black", linetype= 2) -> plt_bS
  }
  
  
  
  if (is.null(gt)){
  layout <- "
  AAABBB
  CCDDEE
  "
  
  plt <- (plt_bA + plt_pA_diff + plt_bS + plt_dis + plt_dir) + 
    plot_layout(design = layout, guides = 'collect') &  
    theme(legend.position = 'bottom', panel.grid = element_blank())
  
  } else {
  
    plt <- (plt_bA + plt_bS + plt_dis + plt_dir) + scale_alpha(guide = 'none') +
      plot_layout(guides = "collect", ncol = 2)  & 
      theme(legend.position = 'bottom', panel.grid = element_blank())  
      
  }
                                              
  
  return(plt)
}
  
plt_post_prior <- function(post, prior, var, xtitle, gt) {
  
  prior_var = paste("prior", var, sep = "_")
  
  if (var == "p_floor") 
  {
    post %>% 
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(exp(get(prior_var)), .width = c(0.53, 0.97)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "lightgrey", alpha = 0.5) +  
      geom_density(aes(exp(get(var)), fill = condition), alpha = 0.5) +
      scale_x_continuous(xtitle) +
      coord_cartesian(xlim = c(0, 0.1)) -> plt
    
  } else {
    post %>% 
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(get(prior_var), .width = c(0.53, 0.97)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "lightgrey", alpha = 0.5) +  
      geom_density(aes(get(var), fill = condition), alpha = 0.5) +
      geom_vline(xintercept = gt, linetype = 2, colour = "black") +
      scale_x_continuous(xtitle) -> plt
    
  }
  
  return(plt)
  
}


plot_traceplots <- function(m)
{
  
  traceplot(m, pars = c("cW", "bS", "b", "sig_cw", "sig_switch"))
  
}


