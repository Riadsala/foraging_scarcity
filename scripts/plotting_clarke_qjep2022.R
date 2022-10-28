library(tidyverse)
library(rstan)
library(patchwork)
library(tidybayes)
library(ggdark)

theme_set(dark_theme_bw())

d <- read_csv("clarke/subset_clarke.csv")
m <- readRDS("clarke/clarke_model.rds")

source("../functions/plot_model.R")

plot_model_fixed(m, d)

plot_init_sel(m)
plot_model_spatial(m)
