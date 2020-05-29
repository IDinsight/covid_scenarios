# Helper functions for modelling/visualising with squire

library(squire)
library(tidyverse)
library(ggplot2)

# Return features with which to build modelling visualisations
# from model output (e.g. summary average and CI values).

plot_prep <- function(x, # simulation object
                      var_select = 'infections',
                      q = c(0.025, 0.975),
                      summary_f = mean,
                      x_var = 't',
                      ...) {
  
  pd <- format_output(x, var_select = var_select, ...)
  
  pd <- pd %>%
    dplyr::mutate(x = .data[[x_var]])
  
  # Apparently need to prevent t from being rounded weirdly
  if(x_var == "t") {
    pd$x <- round(pd$x, ceiling(log10(1/x$parameters$dt)))
  }
  
  # remove any NA rows (due to different start dates)
  if(sum(is.na(pd$t) | is.na(pd$y))>0) {
    pd <- pd[-which(is.na(pd$t) | is.na(pd$y)),]
  }
  
  # Format summary data
  pds <- pd %>%
    dplyr::group_by(.data$x, .data$compartment) %>%
    dplyr::summarise(ymin = stats::quantile(.data$y, q[1]),
                     ymax = stats::quantile(.data$y, q[2]),
                     y = summary_f(.data$y))
  
  return(list(pd = pd, pds = pds))
  
}


