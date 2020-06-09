# This is the main script for running the squire model
# It uses the same backend functions as app.R

#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)

source("./load_data.R")
source("./squire_backend.R")


#-----------------------------------------------#
# Setup environment and data                    #
#-----------------------------------------------#
set.seed(212)
future::plan(future::multiprocess())

# parameters
FORECAST_DAYS = 7
R0_changes <- list("2020-03-24" = 0.2,
                   "2020-05-03" = 0.5)

params <- get_model_params(R0_changes, 'DL')

#-----------#
# Run model #
#---------- #
model <- get_calibrated_model(params)
projection_params <- list(r = model, time_period = FORECAST_DAYS)
future_preds <- get_projection(projection_params)

today <- data.frame(date = Sys.Date(), event = "Today")
plotting_dates <- data.frame(date=as.Date(params[['date_R0_change']]),
                             event=c("Lockdown 1.0", "Lockdown 3.0"))

# Generate plots
get_daily_death_plot(model)
get_hospital_bed_plot(future_preds, max(params$data$date), plotting_dates)
get_icu_plot(future_preds, max(params$data$date), plotting_dates) 
