# This is the main script for running the squire model
# It uses the same backend functions as app.R

#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)
library(cowplot)

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
                   "2020-05-03" = 0.4,
                   "2020-06-06" = 0.5)

params <- get_model_params(R0_changes, 'DL')
params['n_particles'] = 100
params['replicates'] = 100

delhi_govt_proj <- as.data.frame(list("Dates" = c("2020-06-15","2020-06-30","2020-07-15", "2020-07-31"),
                                      "beds" = c(6600, 15000, 33000, 80000)))
delhi_govt_proj$Dates <- as.Date(delhi_govt_proj$Dates)


#-----------#
# Run model #
#---------- #
model2 <- get_calibrated_model(params)
model_version <- model2

# plot(model_version, var_select = c("Deaths"), particle_fit = TRUE)
# plot(model_version, var_select = c("ICase")) 


today <- data.frame(date = Sys.Date(), event = "Today")
plotting_dates <- data.frame(date=as.Date(params[['date_R0_change']]),
                             event=c("Lockdown 1.0", "Lockdown 3.0", "Unlock 1.0"))
plot(model_version$scan_results, what = 'probability')
plot(model_version, particle_fit = TRUE)
get_daily_death_plot(model_version)

# projections ------

# Scenario 1: Severe 1-month lockdown starting Jun 22; 
# Scenario 2: Severe intermittent half-monthly lockdowns starting Jun 3; 
# Scenario 3: Medium-severity lockdown for 4 months starting Jun 22. 
#             I know we don’t know what “severe” and “medium severity” looks like 
#             (what policies do they entail), but we can at least model the numbers 
#             based on R0 drops - to give them a sense of the options. 
#             For now we can interpret “severe” as similar to phase 1 lockdown and 
#             “medium severe” as phase 3 of the national lockdown 
#             (so R0 drops by 50% and 70% respectively).
            
projection_params_base <- list(r = model_version, time_period = FORECAST_DAYS)

# severe lockdown from Jun 22
projection_scenario_1 <- list(r = model_version, 
                          time_period = FORECAST_DAYS,
                          tt_R0 = c(0, 7, 14, 21), 
                          R0_change = c(0.5, 0.2, 0.2, 0.2))

# intermittent lockdown from Jun 3
projection_scenario_2 <- list(r = model_version, 
                                    time_period = FORECAST_DAYS,
                                    tt_R0 = c(0, 7, 14, 21, 28, 35, 42), 
                                    R0_change = c(0.5, 0.8, 0.8, 0.2, 0.2, 0.8, 0.8))

# medium severity for 4 months from Jun 22
projection_scenario_3 <- list(r = model_version, 
                                   time_period = FORECAST_DAYS,
                                   tt_R0 = c(0, 7, 14, 21), 
                                   R0_change = c(0.5, 0.4, 0.4, 0.4))


all_scenarios <- list("scenario_1" = projection_scenario_1,
                      "scenario_2" = projection_scenario_2,
                      "scenario_3" = projection_scenario_3,
                      "base_scenario" = projection_params_base)

make_scenario_plots(all_scenarios)

# --- infection

projection_params_base[['time_period']] = 7
future_preds <- get_projection(projection_params_base)

projection_plotting(r_list = list(future_preds),
                      scenarios = c(""),
                      add_parms_to_scenarios = FALSE,
                      var_select = c("ICase"),
                      date_0 = max(params$data$date),
                      x_var = "date") +
    theme(legend.position = "none") +
    labs(title = "Number of Infections (symptomatic)")

# -- sensitivity to reporting fraction

params <- get_model_params(R0_changes, 'DL')

plot_list <- list()
i = 1
for (reporting_frac in c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) {
  params$reporting_fraction = reporting_frac
  params['n_particles'] = 50
  params['replicates'] = 50
  model_sensitivity <- get_calibrated_model(params)
  plot_list[[i]] <- get_daily_death_plot(model_sensitivity)
  i = i + 1
}



plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], 
          plot_list[[4]], plot_list[[5]], plot_list[[6]], nrow = 2)