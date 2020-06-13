# This is the main script for running the squire model
# It uses the same backend functions as app.R

#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)

source("./load_data.R")
source("./squire_scripts/squire_backend.R")


#-----------------------------------------------#
# Setup environment and data                    #
#-----------------------------------------------#
set.seed(212)
future::plan(future::multiprocess())

# parameters
FORECAST_DAYS = 60
R0_changes <- list("2020-03-24" = 0.2,
                   "2020-05-03" = 0.3,
                   "2020-06-06" = 0.5)

params <- get_model_params(R0_changes, 'DL')
params['n_particles'] = 200
params['replicates'] = 200

delhi_govt_proj <- as.data.frame(list("Dates" = c("2020-06-15","2020-06-30","2020-07-15", "2020-07-31"),
                                      "beds" = c(6600, 15000, 33000, 80000)))
delhi_govt_proj$Dates <- as.Date(delhi_govt_proj$Dates)


#-----------#
# Run model #
#---------- #
model <- get_calibrated_model(params)

today <- data.frame(date = Sys.Date(), event = "Today")
plotting_dates <- data.frame(date=as.Date(params[['date_R0_change']]),
                             event=c("Lockdown 1.0", "Lockdown 3.0", "Unlock 1.0"))
plot(model$scan_results, what = 'probability')
plot(model, particle_fit = TRUE)
get_daily_death_plot(model)

# projections ------
projection_params_base <- list(r = model, time_period = FORECAST_DAYS)

projection_params_no_change <- list(r = model, 
                          time_period = FORECAST_DAYS,
                          tt_R0 = c(0, 7, 14, 21), 
                          R0_change = c(0.5, 0.5, 0.5, 0.5))

projection_params_relaxing <- list(r = model, 
                                    time_period = FORECAST_DAYS,
                                    tt_R0 = c(0, 7, 14, 21), 
                                    R0_change = c(0.5, 0.6, 0.7, 0.8))
projection_params_slow_relaxing <- list(r = model, 
                                   time_period = FORECAST_DAYS,
                                   tt_R0 = c(0, 7, 14, 21), 
                                   R0_change = c(0.5, 0.5, 0.6, 0.6))

projection_params_lockdown <- list(r = model, 
                                   time_period = FORECAST_DAYS,
                                   tt_R0 = c(0, 7, 14, 21), 
                                   R0_change = c(0.5, 0.4, 0.3, 0.2))

# CHOOSE PROJECTION SCENARIO

projection_params <- projection_params_lockdown


future_preds <- get_projection(projection_params)
plot_ <- projection_plotting(r_list = list(future_preds),
                             scenarios = c(""),
                             add_parms_to_scenarios = FALSE,
                             var_select = c("deaths"),
                             date_0 = max(params$data$date),
                            x_var = "date") + ggtitle(paste(c("Deaths based on (", 
                                                             paste0(projection_params$R0_change, 
                                                                    collapse= ", ")
                                                             , ")"), collapse = ""))

add_elements_deaths(plot_, capacity = future_preds$parameters$ICU_bed_capacity, 
                    dates_to_annotate = plotting_dates) 

get_hospital_bed_plot(future_preds, max(params$data$date), plotting_dates) +
  geom_point(data = delhi_govt_proj, aes(x = Dates, y = beds), color='red')


