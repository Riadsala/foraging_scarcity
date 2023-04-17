d_found <- rename(d_found, condition = "block")
d_stim <- rename(d_stim, condition = "block")

d_list <- prep_data_for_stan(d_found, d_stim)
d_list$prior_mu_phidis <- 10

#d_list$targ_class <- d_list$targ_class-1

m <- stan("../../foraging_spatial/models/foraging_model1.stan", data = d_list,
          chains = 1, iter = 1000)

saveRDS(m, "foraging_pilot.model")




blks_labels <- levels(d_found$condition)

source("../functions/plot_model.R")


plot_model_fixed(m, d_found, blks_labels)