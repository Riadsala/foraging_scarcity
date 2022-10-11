library(tidyverse)
library(rstan)
library(patchwork)
library(tidybayes)
library(ggdark)

theme_set(dark_theme_bw())

d <- read_csv("../scratch/sim_data.csv")
m <- readRDS("../scratch/tmp.model")

source("../functions/plot_model.R")

plot_model_fixed(m)

plot_init_sel(m)
plot_model_spatial(m)
