foraging_model1.stan is a new implementation of the model from Clarke et al (2022, Comp Bio). Changes should be in the most part superficial (cleaner code, improved priors)

foraging_model1b.stan is version that uses cts feature values for the items, rather than 2 level factors. As such, there is no need for a stick/switch paramater. 


foraging_model2.stan is the new model with an improved spatial attention map

foraging_model3.stan is the new model with some form of memory / decay in the weights