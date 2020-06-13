# This module has wrappers around squire functions. These are
# called from the app and the main script.

library(ggplot2)
library(scales)

#' Get the calibrate squire model. At this stage this is 
#' just a thing wrapper around squire:: calibrate but will
#' allow us to change it to using `run_explicit_SEEIR_model` 
#' if desired
#' 
#' @param input_param A named list of parameters to use
#' @return calibrated model
get_calibrated_model <- function(input_params) {
  model <- do.call(squire::calibrate, input_params)
  return(model)
}


#' Get projection. At this stage just a thin wrapper
#' around squire::projections but allows changes without
#' breaking rest of yhe code
#' 
#' @param input_param A named list of params for projections
#' @return squire projections
get_projection <- function(input_params) {
  forecast <- do.call(squire::projections, input_params)
  return(forecast)
}


#' Generate the hospital beds plot
#' 
#' @param forecast Output of `get_projection`
#' @param date_0 the starting date of the projection (usually the last date in data)
#' @param dates_to_annotate Dates to annotate. 
#'        A data.frame with two columns: date, event.
#' @return ggplot object
get_hospital_bed_plot <- function(forecast, date_0, dates_to_annotate) {
  today <- data.frame(date = Sys.Date(), event = "Today")
  plot_ <- projection_plotting(r_list = list(forecast),
                      scenarios = c(""),
                      add_parms_to_scenarios = FALSE,
                      var_select = c("hospital_demand"),
                      date_0 = as.Date(date_0),
                      x_var = "date") +
    labs(title = "Projection for hospital bed demand")
  
  plot_ <- add_common_elements(plot_,
                               forecast$parameters$hosp_bed_capacity, 
                               dates_to_annotate) 
  return(plot_)
}


#' Generate the ICU beds plot
#' 
#' @param forecast Output of `get_projection`
#' @param date_0 the starting date of the projection (usually the last date in data)
#' @param dates_to_annotate Dates to annotate. 
#'        A data.frame with two columns: date, event.
#' @return ggplot object
get_icu_plot <- function(forecast, date_0, dates_to_annotate) {
  
  plot_ <- projection_plotting(r_list = list(forecast),
                      scenarios = c(""),
                      add_parms_to_scenarios = FALSE,
                      var_select = c("ICU_demand"),
                      date_0 = as.Date(date_0),
                      x_var = "date") +
    labs(title = "Projection for ICU bed demand")
  
  plot_ <- add_common_elements(plot_, 
                               forecast$parameters$ICU_bed_capacity, 
                               dates_to_annotate)              
  
  return(plot_)
  
}


#' Add common ggplot layers to plot. Ensures consistency betweens
#' the two plots
#' 
#' @param base_plot a ggplot object
#' @param capacity Where to draw the maximum capacity line
#' @param dates_to_annotate Dates to annotate. 
#'        A data.frame with two columns: date, event.
#' @return ggplot object
add_common_elements <- function(base_plot, capacity, dates_to_annotate) {
  
  today <- data.frame(date = Sys.Date(), event = "Today")
  augmented_plot <- base_plot + 
    ylab("No. of beds") +
    xlab("Date") +
    geom_hline(yintercept = capacity, linetype = 4) +   # show bed capacity line
    annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
             y = capacity * 1.05,
             label = "80% bed capacity", size = 3,
             fontface = 'italic', hjust = 0) +
    geom_vline(data = dates_to_annotate,                  # Add lockdown lines
               mapping = aes(xintercept = date),
               color = 'darkgrey',
               alpha = 0.8,
               size = 1) +
    geom_text(data = dates_to_annotate,                   # Annotate lockdown lines
              mapping = aes(x = date,
                            y = capacity * 0.9,
                            label = event),
              size = 3, angle = 90, vjust = -0.5, hjust = 0.9,
              color = 'darkgrey', alpha = 0.8,
              fontface = 'bold') +
    geom_vline(data = today,
               mapping = aes(xintercept = date),
               color = 'darkmagenta',
               alpha = 0.5,
               size = 1) +
    geom_text(data = today,                          # Annotate today line
              mapping = aes(x = date,
                            y = 0,
                            label = event),
              size = 3, angle = 90, vjust = -0.5, hjust = 0,
              color = 'darkmagenta', alpha = 0.5,
              fontface = 'italic') +
    scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                 date_labels = "%b %d",               # mark x-ticks w/ month, day
                 date_minor_breaks = "1 week"         # unmarked grid lines for each week
    ) +
    theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                     hjust = 1)) +
    scale_y_continuous(n.breaks = 8,labels = comma) + 
    theme(legend.position = "none")    
  
  return(augmented_plot)
}


#' Generate plot of model fit on daily death data
#' 
#' @param model a calibrated model. Output of get_calibrated_model
#' @return ggplot object
get_daily_death_plot <- function(model) {
  
  plot_ <- plot(model, var_select = c("deaths"), particle_fit = TRUE) +          
    labs(title = "Model fit to daily death counts to-date") + 
    scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                 date_labels = "%b %d",               # mark x-ticks w/ month, day
                 limits = as.Date(c("2020-03-07", 
                                    Sys.Date()))) +   # cut off viz at today's date
    theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                     hjust = 1)) +    
    scale_y_continuous(labels = comma) +
    ylim(c(0, max(model$scan_results$inputs$data$deaths) + 
             max(model$scan_results$inputs$data$deaths) * 0.15))
  
  return(plot_)
}

#' Add common ggplot layers to plot for the deaths projection.
#' 
#' @param base_plot a ggplot object
#' @param capacity Where to draw the maximum capacity line
#' @param dates_to_annotate Dates to annotate. 
#'        A data.frame with two columns: date, event.
#' @return ggplot object

add_elements_deaths <- function(base_plot, capacity, dates_to_annotate) {
  
  today <- data.frame(date = Sys.Date(), event = "Today")
  augmented_plot <- base_plot + 
    ylab("No. of deaths per day") +
    xlab("Date") +
    geom_vline(data = dates_to_annotate,                  # Add lockdown lines
               mapping = aes(xintercept = date),
               color = 'darkgrey',
               alpha = 0.8,
               size = 1) +
    geom_text(data = dates_to_annotate,                   # Annotate lockdown lines
              mapping = aes(x = date,
                            y = capacity * 0.9,
                            label = event),
              size = 3, angle = 90, vjust = -0.5, hjust = 0.9,
              color = 'darkgrey', alpha = 0.8,
              fontface = 'bold') +
    geom_vline(data = today,
               mapping = aes(xintercept = date),
               color = 'darkmagenta',
               alpha = 0.5,
               size = 1) +
    geom_text(data = today,                          # Annotate today line
              mapping = aes(x = date,
                            y = 0,
                            label = event),
              size = 3, angle = 90, vjust = -0.5, hjust = 0,
              color = 'darkmagenta', alpha = 0.5,
              fontface = 'italic') +
    scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                 date_labels = "%b %d",               # mark x-ticks w/ month, day
                 date_minor_breaks = "1 week"         # unmarked grid lines for each week
    ) +
    theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                     hjust = 1)) +
    scale_y_continuous(n.breaks = 8,labels = comma) + 
    theme(legend.position = "none")    
  
  return(augmented_plot)
}