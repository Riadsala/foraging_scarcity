library(tidybayes)

options(ggplot2.discrete.colour = ggthemes::ptol_pal()(4),
        ggplot2.discrete.fill = ggthemes::ptol_pal()(4))

plot_model_fixed <- function(m, d, cl, gt=NULL)
{
  # gt is a list with our groundtruth sim parameters. 
  
  m %>% recover_types(d$found) %>%
    spread_draws(bA[block], bS[block], sigma_dis[block], sigma_dir[block]) %>%
    mutate(block = as_factor(block)) %>%
    ungroup() -> post
  
  levels(post$block) <- cl
  
  m %>% recover_types(d$found) %>%
    spread_draws(prior_cW, prior_sW, prior_sigma_dis, prior_sigma_dir) %>%
    ungroup() -> prior
  
  my_widths <- c(0.53, 0.97)
  
 
  # plot class weights
  post %>%
    ggplot() + 
    geom_rect(data = prior %>% 
                median_hdci(prior_cW, .width = c(0.53, 0.97)) %>%
              mutate(.lower = boot::inv.logit(.lower),
                     .upper = boot::inv.logit(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "orange", alpha = 0.25) +
    geom_density(aes(boot::inv.logit(bA), fill = block), alpha = 0.5) +
    scale_x_continuous("class weights") +
    theme(legend.position = "bottom") -> plt_cW
  
  # plot stick-switch param
  post  %>%
    ggplot()  + 
    geom_rect(data = prior %>% 
                median_hdci(prior_sW, .width = c(0.53, 0.97)) %>%
                mutate(.lower = boot::inv.logit(.lower),
                       .upper = boot::inv.logit(.upper)),
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
              fill = "orange", alpha = 0.25) + 
    geom_density(aes(boot::inv.logit(bS), fill = block, alpha = 0.5)) +
    #geom_vline(xintercept = 0.5, linetype = 2) +
    scale_x_continuous("stick probability")  +
    theme(legend.position = "bottom") -> plt_sW
  
  # plot proximity and direction effects
  plt_dis <- plt_post_prior(post, prior, "sigma_dis", "proximity tuning", gt$sig_d)
  plt_dir <- plt_post_prior(post, prior, "sigma_dir", "direction tuning", gt$sig_theta)
 # plt_dir2 <- plt_post_prior(post, prior, "direction_bias", "Hori-Vert Pref") 
  
  
  if (!is.null(gt)) {
    # if we have groundtruth, annotate our plt
    plt_cW + geom_vline(xintercept = gt$pA, linetype = 2) -> plt_cW
    
    plt_sW + geom_vline(xintercept = gt$pS, 
                        colour = "black", linetype= 2) -> plt_sW
  }
  
  plt <- plt_cW + plt_sW + plt_dis + plt_dir +
    plot_layout(guides = "collect", ncol = 4)  & theme(legend.position = 'bottom')
  
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
                fill = "orange", alpha = 0.25) +  
      geom_density(aes(exp(get(var)), fill = block), alpha = 0.5) +
      scale_x_continuous(xtitle) +
      coord_cartesian(xlim = c(0, 0.1))-> plt
    
  } else {
    post %>% 
      ggplot() + 
      geom_rect(data = prior %>% 
                  median_hdci(get(prior_var), .width = c(0.53, 0.97)),
                aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper), 
                fill = "orange", alpha = 0.25) +  
      geom_density(aes(get(var), fill = block), alpha = 0.5) +
      geom_vline(xintercept = gt, linetype = 2, colour = "black") +
      scale_x_continuous(xtitle) -> plt
    
  }
  
  return(plt)
  
}


plot_traceplots <- function(m)
{
  
  traceplot(m, pars = c("cW", "bS", "b", "sig_cw", "sig_switch"))
  
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

sample_beta <- function(c, .draw, dim, a, b) {
  
  x <- seq(0.01, 0.99, 0.01)
  
  return(tibble(c = c, 
                draw = .draw,
                dim = dim,
                x = x,
                z = dbeta(x, shape1 = a, shape2 = b)))
}

plot_init_sel <- function(m, d) {
  
  # now to lambdas
  m %>% recover_types(d$found) %>%
    spread_draws(lambda[person]) %>%
    mutate(person = as_factor(person)) %>%
    ggplot(aes(lambda, person)) +
    stat_pointinterval(colour = "white") -> plt_lambda
  
    gather_draws(m, a_x[c], b_x[c], a_y[c], b_y[c], ndraws = 100) %>%
      separate(.variable, into = c("param", "dim")) %>%
      select(-.chain, -.iteration) %>%
      pivot_wider(names_from = param, values_from = ".value") %>%
      pmap_df(sample_beta) %>%
      mutate(c = as_factor(c)) -> plt_dat
    
    plt <- plt_lambda + plt_beta_comp(plt_dat, "y") + plt_beta_comp(plt_dat, "x")
   
    return(plt)
}

plot_model_spatial <- function(m, d) {
  
  m %>% 
    spread_draws( sigma_dis[block], sigma_dir[block]) %>%
    mutate(block = as_factor(block),
           block = fct_recode(block, scarce = "1", equal = "2")) %>%
    ungroup() -> post
  
  m %>% spread_draws(u[block, person],) %>%
    mutate(param = block %% 4,
           param = if_else(param == 0, 4, param),
           block = (block / 4),
           block = ceiling(block),
           block = as_factor(block),
           block = fct_recode(block, feature = "1", conjunction = "2"),
           param = as_factor(param),
           param = fct_recode(param, sigma_dir = "1", sigma_dis = "2")) %>%
    rename(uz = "u") -> post_u
  
  full_join(post %>%
              pivot_longer(c(sigma_dis, sigma_dir), names_to = "param", values_to = "u"), 
            post_u, 
            by = c("block", ".chain", ".iteration", ".draw", "param")) %>% 
    mutate(uz = u + uz) %>%
    group_by(person, block, param) %>%
    summarise(uz = mean(uz), .groups = "drop") %>%
    pivot_wider(names_from = "param", values_from = "uz") -> post_u

    
  distances <- seq(0, 1, 0.025)
  angles <- seq(0, 2*pi, 0.1)
  
  # draw distance tuning figure for foxed effect
    pmap_dfr(select(post, block, sigma_dis), 
             function(sigma_dis, t, block) tibble(t=t, block = block,
                                                       q = exp(-sigma_dis * t)),
             t = distances) %>%
      group_by(block, t) %>%
      median_hdci(q, .width = c(0.53, 0.97)) %>%
      select(-q, -.point, -.interval) %>%
      unite("interval", .lower, .upper) %>%
      pivot_wider(names_from = ".width", values_from = "interval") %>%
      separate(`0.53`, c("lower53", "upper53"), sep  = "_", convert = T)  %>%
      separate(`0.97`, c("lower97", "upper97"), sep  = "_", convert = T) -> q
    
    # plot fixed effect distance -> weight function HDPI
    q %>%
      ggplot(aes(t)) +
      geom_ribbon(aes(ymin = lower97, ymax = upper97, fill = block), alpha = 0.5) +
      geom_ribbon(aes(ymin = lower53, ymax = upper53, fill = block), alpha = 0.7) +
      scale_x_continuous("distance to target") +
      scale_y_continuous("weighting") +
      ggtitle("fixed effects") -> plt_dis
    
    pmap_dfr(select(post, block, direction_bias), 
             function(direction_bias, a, block) tibble(a=a, block = block,
                                                         q = (1 + direction_bias*cos(4*a))/(direction_bias+1)),
             a = angles) %>%
      group_by(block, a) %>%
      median_hdci(q, .width = c(0.53, 0.97)) %>%
      select(-q, -.point, -.interval) %>%
      unite("interval", .lower, .upper) %>%
      pivot_wider(names_from = ".width", values_from = "interval") %>%
      separate(`0.53`, c("lower53", "upper53"), sep  = "_", convert = T)  %>%
      separate(`0.97`, c("lower97", "upper97"), sep  = "_", convert = T) -> q_direction
    
    # plot fixed effect distance -> weight function HDPI
    q_direction %>%
      ggplot(aes(a)) +
      geom_ribbon(aes(ymin = lower97, ymax = upper97, fill = block), alpha = 0.5) +
      geom_ribbon(aes(ymin = lower53, ymax = upper53, fill = block), alpha = 0.7) +
      scale_x_continuous("distance to target") +
      scale_y_continuous("weighting") +
      ggtitle("fixed effects") -> plt_dir
    
    # now plot indiv prox fall offs:
    pmap_dfr(select(post_u, block, sigma_dis, person), 
             function(sigma_dis, t, block, person) tibble(t=t, block = block, person = person,
                                                         q = exp(-sigma_dis * t)),
             t = distances) %>%
      ggplot(aes(x = t, y = q, colour = block, group = interaction(block, person))) + 
      geom_path(alpha = 0.5) +
      scale_x_continuous("distance to target") +
      scale_y_continuous("weighting") +
      ggtitle("indiv. differences") -> plt_dis_p
    
    
    
     plt_dis + plt_dis_p + plt_dir
  
}
