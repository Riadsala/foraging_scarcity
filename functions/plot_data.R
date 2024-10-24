library(tidyverse)
library(geomtextpath)

# This script contains functions for plotting data (whether empirical or simulated).
# Generally, data is assumed to be stored in ds (data - stimulus) and df (data - found).

plot_a_trial <- function(ds, df, 
                         trial = NA, segLabel = NULL,
                         filename = NA) {

  # This function plots a trial: points indicate all items and a path 
  # joins the items up in the order in which they where selected.
  # Path vertices are labelled to indicate the order in which they were selected
  # Path segments may be labelled with some feature (a column in df).
  
  # if a trial number has been supplied, get data from that trial only
  # if trial = NA, assume that only one trial of data was supplied
  if (is.finite(trial)) {
    trl = trial
    ds <- filter(ds, trial == trl)
    df <- filter(df, trial == trl)
  }
  
  ds %>% mutate(item_class = factor(item_class)) -> ds
  
  # plot basic trial
  plt <- ggplot(data = ds, aes(x, y)) + 
    geom_point(size = 5, aes(colour = item_class, shape = item_class)) +
    ggrepel::geom_label_repel(data = df, aes(label = found), size = 3) + 
    scale_colour_manual(values = c(18, 15, 3, 4)) + 
    scale_shape_manual(values = c(15, 19, 3, 4))
  
  # if segLabel has been left unspecified, add a simple geom_path to connect selected items
  if (is_null(segLabel)) {

    plt <- plt + geom_path(data = df, colour = "grey", group = 1) 
    
  } else { # use geom_labelsegment..
    
    #first we have to rearrange things a little
    df %>% mutate(x0 = lag(x), y0 = lag(y)) -> df
      
    # turn labels into characeters and repace NA with a dash (-)
    df[[segLabel]] = as.character(round(df[[segLabel]], 2))
    df[[segLabel]][is.na(df[[segLabel]])]  = "-"
      
    plt <- plt + geom_textsegment(data = df, 
                                  aes(x = x0, y = y0, 
                                      xend = x, yend = y,
                                      label = .data[[segLabel]]), 
                                  colour = "grey")
  }
  
  plt <- plt + coord_equal() + 
    theme(axis.title = element_blank(),
          axis.ticks  = element_blank(),
          axis.text = element_blank(),
          legend.position = "none",
          plot.background = element_rect(fill='darkgrey', colour='black'))
  
  if (!is.na(filename)) {
    ggsave(filename)
  }
      
  return(plt)
}

