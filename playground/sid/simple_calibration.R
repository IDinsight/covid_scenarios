# SQUIRE model for Uttarkhand
library(tidyverse)
library(squire)

#pop_dist <- read.csv("./data/maharashtra_pop.csv")
pop_dist <- read.csv("./data/uttarkhand_pop.csv") # http://statisticstimes.com/demographics/population-of-uttarakhand.php
pop_vector <- pop_dist$total_persons[-18] %>% 
                    as.character %>% 
                    str_remove_all(",") %>% 
                    as.integer
state_wise_data <- read.csv("./data/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

# Setup data
STATE = 'UT'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))
# Calibrate

#future::plan(future::multiprocess())

int_unique <- list(dates_change = c("2020-03-24"),
                   change = c(0.2))

out <- calibrate(
  data = df,
  R0_min = 0.5,
  R0_max = 7,
  R0_step = 0.5,
  first_start_date = "2020-03-05",
  last_start_date = "2020-03-10",
  day_step = 1,
  replicates = 100,
  n_particles = 100,
  population = pop_vector,
  forecast = 14,
  baseline_contact_matrix  = india_params_list$contact_matrix_set,
  R0_change = int_unique$change,
  date_R0_change = int_unique$dates_change
)

plot(out, particle_fit = TRUE) + ggtitle(label = "Uttarakhand")

plot(out, "deaths")

Some info we'll need:
reporting_fraction: what proportion of the total deaths the reported deaths
ICU_bed_capacity:  And how that has changed over time
hospital_bed_capacity: And how that has changed over time
p_dist: Preferentiality of age group receiving treatment relative to other age groups when demand exceeds healthcare capacity. Probably in Raghav's doc. Have to check