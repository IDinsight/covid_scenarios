# SQUIRE model for NCT Delhi

library(squire)
library(tidyverse)
library(ggplot2)

pop_dist <- read.csv("./data/delhi_pop.csv")

pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
  as.character %>% 
  str_remove_all(",") %>% 
  as.integer
state_wise_data <- read.csv("./data/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

# Setup data
STATE = 'DL'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))

int_unique <- list(dates_change = c("2020-03-24"), # Day of lockdown
                   change = c(0.2))

out <- calibrate(
  data = df,
  R0_min = 0.5,
  R0_max = 7,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-10",
  day_step = 1,
  replicates = 10,
  n_particles = 100,
  population = pop_vector,
  forecast = 14,
  baseline_contact_matrix  = india_params_list$contact_matrix_set,
  R0_change = int_unique$change,
  date_R0_change = int_unique$dates_change
)

# Save model locally to avoid re-running grid search in new sessions
#save(out, file = "/tmp/delhi_out_1205.rda") # this doesn't work??
# Reload model if applicable
#out <- load(file = "/tmp/delhi_out_1205.rda")

# Plot with particle fit
plot(out, 'deaths', particle_fit = TRUE) + 
  ggtitle(label = "Delhi")

# Note date of national lockdown
natl_lockdown <- as.numeric(as.Date("2020-03-24"))

# Plot Delhi infections
plot(out, 'infections', date_0 = max(df$date), x_var = "date") + 
  labs(title = "Delhi infections",
       subtitle = "[subtitle]",
       caption = "[caption]") +
  ylab("Infections") +
  geom_vline(xintercept = natl_lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 800, 
           label = "Nat'l lock-\ndown begins", size = 3, 
           fontface = 'italic', hjust = 1) +
  theme(legend.position = "none")  # suppress legend


